import { AutocompleteData, NS, ScriptArg } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";
import { rip } from "./rip";

type Target = { server: string, stock: boolean };

const MaxNonStockTargets = 5;

const homeReserve = 96;
const batchSize = 100_000;

const defaultHGW = { hack: 1, grow: 8, weaken: 2 };
// const defaultHGW = { hack: 0, grow: 1, weaken: 0 };

export type SymbolDict = { [key: string]: string | undefined };

export const serverSymbols: SymbolDict = {};

// eslint-disable-next-line @typescript-eslint/no-unused-vars
type HGWOutcome = {
    timeElapsed: number,
    moneyYield: number,
    deltaExp: number,
    deltaSec: number,
    serverMoneyPercent: number,
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function hackFraction(hgw: HGWRatio): number {
    return hgw.hack / (hgw.hack + hgw.grow + hgw.weaken);
}

function growFraction(hgw: HGWRatio): number {
    return hgw.grow / (hgw.hack + hgw.grow + hgw.weaken);
}

function weakenFraction(hgw: HGWRatio): number {
    return hgw.weaken / (hgw.hack + hgw.grow + hgw.weaken);
}

type HGWRatio = { hack: number, grow: number, weaken: number }

async function stagger(ns: NS, threads: number) {
    await ns.sleep(1000 * threads / batchSize);
    return;
}

function balanceHGW(ns: NS, server: string, pool: ThreadPool, hgw: HGWRatio): HGWRatio {
    return hgw;
}

async function getHGW(ns: NS, server: string, pool: ThreadPool): Promise<HGWRatio> {
    const hgw = defaultHGW;
    // TODO - use formulas API to calculate ideal HGW
    return balanceHGW(ns, server, pool, hgw);
}



class ProcessInfo {
    protected pidThreads: Map<number, number>;

    constructor() {
        this.pidThreads = new Map();
    }

    public addProcess(pid: number, threads: number) {
        this.pidThreads.set(pid, threads);
    }

    public getThreadCount(pid: number): number {
        return this.pidThreads.get(pid) ?? 0;
    }

    /**
     * Removes a process identifier and returns how many threads it had, if any.
     * @param pid
     */
    public pruneProcess(pid: number): number {
        const ret = this.pidThreads.get(pid) ?? 0;
        this.pidThreads.delete(pid);
        return ret;
    }

    get pids(): number[] {
        return [...this.pidThreads.keys()];
    }
}

type ThreadInfo = ThreadSummary & { hacks: ProcessInfo, grows: ProcessInfo, weakens: ProcessInfo };

type ThreadSummary = {
    maxThreads: number,
    hackThreads: number,
    growThreads: number,
    weakenThreads: number,
};

export class ThreadPool {
    public isInitialized = false;
    protected pool: Map<string, ThreadInfo>;
    protected readonly threadRam;

    constructor(ns: NS) {
        this.pool = new Map();
        this.threadRam = Math.max(ns.getScriptRam("hack.js", "home"), ns.getScriptRam("grow.js", "home"), ns.getScriptRam("weaken.js", "home"));
    }

    public knownServers(): string[] {
        return [...this.pool.keys()];
    }

    public init(ns: NS, graph: NodeInfo[]): void {
        if (this.isInitialized) return;
        for (const server of graph) {
            if (!ns.serverExists(server.name)) {
                continue;
            }
            if (ns.hasRootAccess(server.name) || canHack(ns, server.name, 1)) {
                let freeRam = ns.getServerMaxRam(server.name) - ns.getServerUsedRam(server.name);
                if (server.name == "home") {
                    freeRam = Math.max(freeRam - homeReserve, 0);
                }
                const nThreads = Math.floor(freeRam / this.threadRam);
                this.addNewServer(server.name, nThreads);
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
                const usedRam = ns.getServerUsedRam(server.name);
                const freeRam = Math.max(((server.name === "home") ? maxRam - homeReserve : maxRam) - usedRam, 0);
                const nThreads = Math.floor(freeRam / this.threadRam);
                this.addNewServer(server.name, nThreads);
            } else {
                const maxRam = ns.getServerMaxRam(server.name);
                const usedRam = ns.getServerUsedRam(server.name);
                const freeRam = Math.max(((server.name === "home") ? maxRam - homeReserve : maxRam) - usedRam, 0);
                const nThreads = Math.floor(freeRam / this.threadRam);
                this.updateServerThreads(ns, server.name, nThreads);
            }
        }
    }

    public addHackThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        serverInfo.hackThreads += nThreads;
        serverInfo.hacks.addProcess(pid, nThreads);
    }

    public addGrowThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        serverInfo.growThreads += nThreads;
        serverInfo.grows.addProcess(pid, nThreads);
    }

    public addWeakenThreads(server: string, nThreads: number, pid: number): void {
        const serverInfo = this.pool.get(server);
        if (serverInfo === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        serverInfo.weakenThreads += nThreads;
        serverInfo.weakens.addProcess(pid, nThreads);
    }

    getInfo(server: string) {
        const ret = this.pool.get(server);
        if (ret === undefined) {
            throw new Error(`Server ${server} not in pool`);
        }
        return ret;
    }

    public addNewServer(server: string, nThreads: number): void {
        if (this.pool.has(server)) {
            return;
        }
        this.pool.set(server, {
            maxThreads: nThreads,
            hackThreads: 0,
            growThreads: 0,
            weakenThreads: 0,
            hacks: new ProcessInfo(),
            weakens: new ProcessInfo(),
            grows: new ProcessInfo(),
        });
    }

    public updateServerThreads(ns: NS, server: string, newThreads: number): void {
        const serverInfo = this.pool.get(server);

        if (serverInfo === undefined) {
            return this.addNewServer(server, newThreads);
        }

        serverInfo.maxThreads = Math.max(newThreads, serverInfo.hackThreads + serverInfo.weakenThreads + serverInfo.growThreads);

        const hackPids = serverInfo.hacks.pids;
        const growPids = serverInfo.grows.pids;
        const weakenPids = serverInfo.weakens.pids;

        let nHackThreads = 0;
        let nGrowThreads = 0;
        let nWeakenThreads = 0;

        for (const pid of hackPids) {
            if (ns.isRunning(pid)) {
                nHackThreads += serverInfo.hacks.getThreadCount(pid);
            } else {
                serverInfo.hacks.pruneProcess(pid);
            }
        }

        for (const pid of growPids) {
            if (ns.isRunning(pid)) {
                nGrowThreads += serverInfo.grows.getThreadCount(pid);
            } else {
                serverInfo.grows.pruneProcess(pid);
            }
        }

        for (const pid of weakenPids) {
            if (ns.isRunning(pid)) {
                nWeakenThreads += serverInfo.weakens.getThreadCount(pid);
            } else {
                serverInfo.weakens.pruneProcess(pid);
            }
        }

        serverInfo.hackThreads = nHackThreads;
        serverInfo.growThreads = nGrowThreads;
        serverInfo.weakenThreads = nWeakenThreads;
    }

    public size(): number {
        return [...this.pool.values()].reduce((acc, server) => acc + server.maxThreads, 0);
    }

    public headCount(): ThreadSummary {
        return [...this.pool.values()].reduce((acc, server) => {
            acc.maxThreads += server.maxThreads;
            acc.hackThreads += server.hackThreads;
            acc.growThreads += server.growThreads;
            acc.weakenThreads += server.weakenThreads;
            return acc;
        }, { maxThreads: 0, hackThreads: 0, growThreads: 0, weakenThreads: 0 });
    }
}

function renew(ns: NS): NodeInfo[] {
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

async function hideLogs(ns: NS) {
    ns.disableLog("disableLog");
    ns.disableLog("killall");
    ns.disableLog("scriptKill");
    ns.disableLog("getServerUsedRam");
    ns.disableLog("exec");
    ns.disableLog("scp");
    ns.disableLog("getServerMaxRam");
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("getServerMaxMoney");
    ns.disableLog("getServerMinSecurityLevel");
    ns.disableLog("getServerSecurityLevel");
    ns.disableLog("getServerRequiredHackingLevel");
    ns.disableLog("getServerNumPortsRequired");
    ns.disableLog("getHackingLevel");
    ns.disableLog("scan");
    ns.disableLog("sleep");
}

function decideTarget(ns: NS, args: ScriptArg | string[]): [string, boolean] {
    function getTargets(): Target[] {
        const targets = [];
        let nNonStock = 0;
        const profitDescending = rip(ns);
        for (const candidate of profitDescending) {
            const asTarget: Target = { server: candidate[0], stock: shouldStock(ns, candidate[0]) };
            if (canHack(ns, asTarget.server)) {
                if (asTarget.stock || nNonStock < MaxNonStockTargets) {
                    targets.push(asTarget);
                    if (!asTarget.stock) nNonStock++;
                }
            }
        }
        return targets;
    }

    let targets;
    let userpick;
    const cmdLineTarget = (args as string[])[0];
    if (typeof cmdLineTarget === "string") {
        targets = [{ server: cmdLineTarget, stock: shouldStock(ns, cmdLineTarget) }]
        userpick = true;
    } else {
        targets = getTargets();
        userpick = false;
    }

    const targetObj = pickTarget(targets);
    const target = targetObj.server;
    return [target, userpick];
}

async function spawnTarget(ns: NS, target: string, graph: NodeInfo[], args: string[] | ScriptArg, userPicked = false, runOnHacknet = false): Promise<string | -1> {
    ns.write("target.txt", target, "w");
    ns.write("dispatch-pid.txt", ns.pid.toString() ?? "0", "w");

    const pool = new ThreadPool(ns);
    graph = filterGraph(graph, runOnHacknet)
    pool.init(ns, graph);
    ns.tprint(`Initialized pool with ${pool.size()} threads across ${pool.knownServers().length} servers`);
    const hgw = await getHGW(ns, target, pool);

    for (; ;) {
        const minSec = ns.getServerMinSecurityLevel(target);
        const headCount = pool.headCount();
        ns.print(`Threads: ${headCount.hackThreads} hack, ${headCount.growThreads} grow, ${headCount.weakenThreads} weaken`);
        const available = headCount.maxThreads - (headCount.hackThreads + headCount.growThreads + headCount.weakenThreads);
        ns.print(`Available: ${available}`);

        const idealTotalWeaken = Math.ceil(headCount.maxThreads * weakenFraction(hgw));
        const idealTotalGrow = Math.floor(headCount.maxThreads * growFraction(hgw));
        const idealTotalHack = headCount.maxThreads - idealTotalWeaken - idealTotalGrow;

        let newWeaken = Math.max(0, idealTotalWeaken - headCount.weakenThreads);
        let newGrow = Math.max(0, idealTotalGrow - headCount.growThreads);
        let newHack = Math.max(0, idealTotalHack - headCount.hackThreads);

        if (newHack + newGrow + newWeaken > 0) {
            ns.print(`New threads: ${newHack} hack, ${newGrow} grow, ${newWeaken} weaken`);
            const graphNames = graph.map((node) => node.name);
            servers: for (const server of graphNames) {
                if (!ns.serverExists(server)) continue servers;
                if (!ns.hasRootAccess(server)) continue servers;
                const info = pool.getInfo(server);
                const newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
                let remainingThreads = newServerThreads;
                while (remainingThreads > 0) {
                    let spawnedThreads = 0;
                    const batchGrow = Math.min(Math.ceil(growFraction(hgw) * batchSize), newGrow);
                    const batchWeaken = Math.min(Math.ceil(weakenFraction(hgw) * batchSize), newWeaken);
                    const batchHack = Math.min(Math.ceil(hackFraction(hgw) * batchSize), newHack);

                    if (batchWeaken > 0) {
                        const nWeaken = batchWeaken;
                        let threads = remainingThreads;
                        if (threads > nWeaken) {
                            threads = nWeaken;
                        }
                        if (threads > 0) {
                            if (!ns.fileExists("weaken.js", server)) {
                                if (server === "home") {
                                    ns.tprint("ERROR: Failed to find weaken.js in home");
                                    return -1;
                                }
                                const res = ns.scp("weaken.js", server, "home");
                                if (!res) {
                                    ns.tprint(`ERROR: Failed to scp weaken.js to ${server}`);
                                    return -1;
                                }
                            }
                            const pid = ns.exec("weaken.js", server, { threads }, target);
                            if (pid !== 0) {
                                spawnedThreads += threads;
                                pool.addWeakenThreads(server, threads, pid);
                                newWeaken -= threads;
                                remainingThreads -= threads;
                            }
                        }
                    }
                    if (batchGrow > 0) {
                        const nGrow = batchGrow;
                        let threads = remainingThreads;
                        if (threads > nGrow) {
                            threads = nGrow;
                        }
                        if (threads > 0) {
                            if (!ns.fileExists("grow.js", server)) {
                                if (server === "home") {
                                    ns.tprint("ERROR: Failed to find grow.js in home");
                                    return -1;
                                }
                                const res = ns.scp("grow.js", server, "home");
                                if (!res) {
                                    ns.tprint(`ERROR: Failed to scp grow.js to ${server}`);
                                    return -1;
                                }
                            }
                            const pid = ns.exec("grow.js", server, { threads }, target, shouldStock(ns, target));
                            if (pid !== 0) {
                                spawnedThreads += threads;
                                pool.addGrowThreads(server, threads, pid);
                                newGrow -= threads;
                                remainingThreads -= threads;
                            }
                        }
                    }
                    if (batchHack > 0) {
                        const nHack = batchHack;
                        let threads = remainingThreads;
                        if (threads > nHack) {
                            threads = nHack;
                        }
                        if (threads > 0) {
                            if (server !== "home") {
                                const res = ns.scp("hack.js", server, "home");
                                if (!res) {
                                    ns.print(`Failed to scp hack.js to ${server}`);
                                }
                            }
                            const pid = ns.exec("hack.js", server, { threads }, target);
                            if (pid !== 0) {
                                spawnedThreads += threads;
                                pool.addHackThreads(server, threads, pid);
                                newHack -= threads;
                                remainingThreads -= threads;
                            }
                        }
                    }
                    if (spawnedThreads == 0) {
                        continue servers;
                    }
                    ns.print(`Spawned a total of ${spawnedThreads} threads on server ${server} (${batchHack}, ${batchGrow}, ${batchWeaken} HGW)`);
                    await stagger(ns, spawnedThreads);
                }
            }
        }

        await ns.sleep(1000);
        graph = filterGraph(getGraph(ns, true), runOnHacknet);
        const currentSec = ns.getServerSecurityLevel(target);
        const currentMoney = ns.getServerMoneyAvailable(target);
        ns.clearLog();
        ns.print(`${target} Snapshot: Security = ${currentSec.toFixed(2)} (Min: ${minSec.toFixed(2)}), Money = $${ns.formatNumber(currentMoney)}`);
        pool.update(ns, graph);
        if (!userPicked) {
            const [nextTarget] = decideTarget(ns, args);
            if (nextTarget !== target) {
                return nextTarget;
            }
        }
    }
}


export async function main(ns: NS): Promise<void> {
    const flags = ns.flags([["hacknet", false]]);
    hideLogs(ns);

    const graph = renew(ns);
    const [initTarget, userPicked] = decideTarget(ns, flags._);
    ns.tail();

    let target = initTarget;
    for (; ;) {
        const newTarget = await spawnTarget(ns, target, graph, flags._, userPicked, flags.hacknet as boolean);
        if (newTarget === -1) {
            return;
        } else if (newTarget !== target) {
            target = newTarget;
            renew(ns);
            continue;
        }
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]): string[] {
    data.flags([["hacknet", false]]);
    return [...data.servers]
}

function serverToSymbol(ns: NS, server: string): string | undefined {
    if (typeof serverSymbols[server] === "string") {
        return serverSymbols[server];
    }
    const serverInfo = ns.getServer(server);
    const orgName = serverInfo.organizationName;
    if (typeof orgName === "string" && orgName !== "") {
        for (const sym of ns.stock.getSymbols()) {
            if (ns.stock.getOrganization(sym) == orgName) {
                serverSymbols[server] = sym;
                return sym;
            }
        }
    }
    return undefined;
}

function shouldStock(ns: NS, server: string): boolean {
    if (!ns.stock.hasTIXAPIAccess()) {
        return false;
    }
    const sym = serverToSymbol(ns, server);
    const stock = (sym !== undefined && ns.stock.getPosition(sym)[0] > 0);
    return stock;
}

function pickTarget(targets: Target[]): Target {
    if (targets.length === 0) {
        return { server: "n00dles", stock: false };
    }
    const stockTargets = targets.filter(t => t.stock);
    if (stockTargets.length > 0) {
        return stockTargets[0];
    }
    return targets[0];
}

function filterGraph(graph: NodeInfo[], keepHacknet = false): NodeInfo[] {
    if (keepHacknet) {
        return graph;
    } else {
        return graph.filter((info) => !info.name.startsWith("hacknet-server"));
    }
}
