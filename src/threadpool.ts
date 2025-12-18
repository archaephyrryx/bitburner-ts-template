import { NS, ScriptArg } from "@ns";
import { getGraph } from "./census";
import { NodeInfo } from "./global";
import { canHack } from "./helper";

// Target server to run hack/grow/weaken batch over, with flag for whether stock price should be affected
type Target = { server: string, stock: boolean };

// GB of RAM to reserve on home server for scripting purposes
const homeReserve = 128;

// Class for tracking process identifiers and their associated thread counts
class ProcessInfo {
    // Maps process identifier to number of threads
    protected pidThreads: Map<number, number>;

    constructor() {
        this.pidThreads = new Map();
    }

    /**
     * Adds a process identifier and its associated thread count.
     * @param pid process identifier
     * @param threads number of threads associated with the process
     */
    public addProcess(pid: number, threads: number) {
        this.pidThreads.set(pid, threads);
    }

    /**
     * Returns the number of threads associated with the given PID.
     * @param pid process identifier to look up
     * @returns number of threads associated with the given PID, or 0 if none
     */
    public getThreadCount(pid: number): number {
        return this.pidThreads.get(pid) ?? 0;
    }

    /**
     * Removes a process identifier and returns how many threads it had, if any.
     *
     * Should only be called once per PID, as soon as the PID is known to no longer be running.
     *
     * @param pid: process identifier to remove
     */
    public pruneProcess(pid: number): number {
        const ret = this.pidThreads.get(pid) ?? 0;
        this.pidThreads.delete(pid);
        return ret;
    }

    get pids(): number[] {
        return [...this.pidThreads.keys()];
    }

    public countAllThreads(ns: NS): number {
        let total = 0;
        const pruneList = [];
        for (const [pid, threads] of this.pidThreads.entries()) {
            if (ns.isRunning(pid)) {
                total += threads;
            } else {
                pruneList.push(pid);
            }
        }
        if (pruneList.length > 0) {
            for (const pid of pruneList) {
                this.pruneProcess(pid);
            }
        }
        return total;
    }
}

/**
 * Summary of the individual task-threads running on a specific server
 */
export type PerServerInfo = { maxRam: number, otherRam: number } & { [key in TaskName]?: ProcessInfo };

/**
 * Summary of the distribution of threads across all servers for a single task (hack/grow/weaken)
 */
export type PerTaskInfo = { [key in ServerName]: ProcessInfo };

export type TaskName = "hack" | "grow" | "weaken";
type ServerName = string;

export type ThreadSummary = { hack: number, grow: number, weaken: number };

/**
 * Manager class for distributing threads across multiple servers
 */
export class ThreadPool {
    public isInitialized = false;
    protected pool: Map<ServerName, PerServerInfo>;
    protected tasks: Map<TaskName, PerTaskInfo>;
    protected readonly threadRam: { [key in TaskName]: number };

    constructor(ns: NS) {
        this.pool = new Map();
        this.tasks = new Map();
        this.threadRam = {
            hack: ns.getScriptRam("hack.js", "home"),
            grow: ns.getScriptRam("grow.js", "home"),
            weaken: ns.getScriptRam("weaken.js", "home"),
        };
    }

    public knownServers(): string[] {
        return [...this.pool.keys()];
    }

    public serversByRamDescending(): string[] {
        return [...this.pool.entries()]
            .sort(([, infoA], [, infoB]) => (infoB.maxRam - infoB.otherRam) - (infoA.maxRam - infoA.otherRam))
            .map(([name]) => name);
    }

    public init(ns: NS, graph: NodeInfo[]): void {
        if (this.isInitialized) return;
        for (const server of graph) {
            if (!ns.serverExists(server.name)) {
                continue;
            }
            if (ns.hasRootAccess(server.name) || canHack(ns, server.name, 1)) {
                const maxRam = ns.getServerMaxRam(server.name);
                let otherRam = ns.getServerUsedRam(server.name);
                if (otherRam < homeReserve && server.name === "home") {
                    otherRam = homeReserve;
                }
                this.addNewServer(server.name, maxRam, otherRam);
            }
        }
        this.isInitialized = true;
    }

    public update(ns: NS, graph: NodeInfo[]): void {
        for (const server of graph) {
            const poolServer = this.pool.get(server.name);
            if (poolServer === undefined) {
                if (!ns.serverExists(server.name) || !ns.hasRootAccess(server.name)) {
                    continue;
                }
                const maxRam = ns.getServerMaxRam(server.name);
                let otherRam = ns.getServerUsedRam(server.name);
                if (otherRam < homeReserve && server.name === "home") {
                    otherRam = homeReserve;
                }
                this.addNewServer(server.name, maxRam, otherRam);
            } else {
                this.updateServerInfo(ns, server.name);
            }
        }
    }

    public addHackThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        const hackPool = this.tasks.get("hack");
        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        if (serverInfo.hack === undefined) {
            serverInfo.hack = new ProcessInfo();
        }
        serverInfo.hack.addProcess(pid, nThreads);
        hackPool![server] = serverInfo.hack;
    }

    public addGrowThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        const growPool = this.tasks.get("grow");

        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        if (serverInfo.grow === undefined) {
            serverInfo.grow = new ProcessInfo();
        }
        serverInfo.grow.addProcess(pid, nThreads);
        growPool![server] = serverInfo.grow;
    }

    public addWeakenThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        const weakenPool = this.tasks.get("weaken");

        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        if (serverInfo.weaken === undefined) {
            serverInfo.weaken = new ProcessInfo();
        }
        serverInfo.weaken.addProcess(pid, nThreads);
        weakenPool![server] = serverInfo.weaken;
    }

    getInfo(server: string): PerServerInfo {
        const ret = this.pool.get(server);
        if (ret === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        return ret;
    }

    public addNewServer(server: string, maxRam: number, otherRam: number): void {
        if (this.pool.has(server)) {
            return;
        }
        this.pool.set(server, {
            maxRam: maxRam,
            otherRam: otherRam,
            hack: new ProcessInfo(),
            weaken: new ProcessInfo(),
            grow: new ProcessInfo(),
        });
    }

    public updateServerInfo(ns: NS, server: string): void {
        const serverInfo = this.pool.get(server);

        if (serverInfo === undefined) {
            const maxRam = ns.getServerMaxRam(server);
            const otherRam = server == "home" ? homeReserve : ns.getServerUsedRam(server);
            return this.addNewServer(server, maxRam, otherRam);
        }

        const taskThreads: { [key in TaskName]: number } = {
            hack: 0,
            grow: 0,
            weaken: 0,
        };

        for (const task of ["hack", "grow", "weaken"] as TaskName[]) {
            if (serverInfo[task] === undefined) {
                serverInfo[task] = new ProcessInfo();
            } else {
                for (const pid of serverInfo[task].pids) {
                    if (ns.isRunning(pid)) {
                        taskThreads[task] += serverInfo[task].getThreadCount(pid);
                    } else {
                        serverInfo[task].pruneProcess(pid);
                    }
                }
            }
        }
        const usedRam = ns.getServerUsedRam(server);
        const otherRam = usedRam - (taskThreads.hack * this.threadRam.hack + taskThreads.grow * this.threadRam.grow + taskThreads.weaken * this.threadRam.weaken);

        serverInfo.otherRam = otherRam;
    }

    public poolRam(): number {
        return [...this.pool.values()].reduce((acc, server) => acc + server.maxRam - server.otherRam, 0);
    }

    public headCount(ns: NS): ThreadSummary {
        const summary = { hack: 0, grow: 0, weaken: 0 };
        for (const server of this.pool.values()) {
            for (const task of ["hack", "grow", "weaken"] as TaskName[]) {
                if (server[task] !== undefined) {
                    summary[task] += server[task].countAllThreads(ns);
                }
            }
        }
        return summary;
    }
}

export function generateThreadPool(ns: NS): ThreadPool {
    const pool = new ThreadPool(ns);
    let graph = clearTaskThreads(ns);
    pool.init(ns, graph);
    return pool;
}

export function spawnTaskThreads(ns: NS,  pool: ThreadPool, task: TaskName, totalThreads: number, targetServer: ServerName, allTogether: boolean = true): number {
    let remainingThreads = totalThreads;
    const servers = pool.serversByRamDescending();
    for (const server of servers) {
        const serverInfo = pool.getInfo(server);
        const availableRam = serverInfo.maxRam - ns.getServerUsedRam(server);
        const threadRam = ns.getScriptRam(`${task}.js`, "home");
        if (availableRam >= threadRam * remainingThreads) {
            if (!ns.fileExists(`${task}.js`, server)) {
                if (ns.fileExists(`${task}.js`, "home")) {
                    ns.scp(`${task}.js`, server, "home");
                } else {
                    return -1;
                }
            }
            const pid = ns.exec(`${task}.js`, server, { threads: remainingThreads }, targetServer);
            if (pid === 0) {
                ns.alert(`ERROR: Failed to exec ${task}.js on ${server}`);
                return -1;
            }
            switch (task) {
                case "hack":
                    pool.addHackThreads(server, remainingThreads, pid);
                    break;
                case "grow":
                    pool.addGrowThreads(server, remainingThreads, pid);
                    break;
                case "weaken":
                    pool.addWeakenThreads(server, remainingThreads, pid);
                    break;
            }
            return pid;
        } else if (availableRam >= threadRam && !allTogether) {
            const nThreads = Math.floor(availableRam / threadRam);
            if (!ns.fileExists(`${task}.js`, server)) {
                if (ns.fileExists(`${task}.js`, "home")) {
                    ns.scp(`${task}.js`, server, "home");
                } else {
                    return -1;
                }
            }
            const pid = ns.exec(`${task}.js`, server, { threads: nThreads }, targetServer);
            if (pid === 0) {
                ns.alert(`ERROR: Failed to exec ${task}.js on ${server}`);
                return -1;
            }
            switch (task) {
                case "hack":
                    pool.addHackThreads(server, nThreads, pid);
                    break;
                case "grow":
                    pool.addGrowThreads(server, nThreads, pid);
                    break;
                case "weaken":
                    pool.addWeakenThreads(server, nThreads, pid);
                    break;
            }
            remainingThreads -= nThreads;
            return pid;
        } else {
            continue;
        }
    }

    ns.alert(`ERROR: Not enough RAM in pool to spawn ${totalThreads} ${task} threads for ${targetServer}`);
    return 0;
}

function clearTaskThreads(ns: NS): NodeInfo[] {
    const graph = getGraph(ns, true);
    ns.scriptKill("hack.js", "home");
    ns.scriptKill("grow.js", "home");
    ns.scriptKill("weaken.js", "home");
    for (const serverInfo of graph) {
        if (serverInfo.name === "home") {
            continue;
        }
        if (!ns.serverExists(serverInfo.name)) {
            continue;
        }
        if (ns.hasRootAccess(serverInfo.name)) {
            ns.killall(serverInfo.name, true);
        }
    }
    return graph;
}
