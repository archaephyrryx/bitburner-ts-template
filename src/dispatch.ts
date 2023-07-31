import { NS } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";

function isProfitable(ns: NS, server: string): boolean {
    if (ns.getServerMaxMoney(server) <= 0) {
        return false;
    }
    return true;
}

const homeReserve = 16;

const hgw = { hack: 1, grow: 10, weaken: 2 };

type ThreadInfo = { maxThreads: number, hackThreads: number, growThreads: number, weakenThreads: number };

export class ThreadPool {
    public isInitialized = false;
    public pool: { [server: string]: ThreadInfo };
    protected readonly threadRam;

    constructor(ns: NS) {
        this.pool = {};
        this.threadRam = ns.getScriptRam("hack.js", "home");
    }

    public init(ns: NS, graph: NodeInfo[]): void {
        if (this.isInitialized) return;
        for (const server of graph) {
            if (ns.hasRootAccess(server.name)) {
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
            if (ns.hasRootAccess(server.name) && !(server.name in Object.keys(this.pool))) {
                let freeRam = ns.getServerMaxRam(server.name) - ns.getServerUsedRam(server.name);
                if (server.name == "home") {
                    freeRam = Math.max(freeRam - homeReserve, 0);
                }
                const nThreads = Math.floor(freeRam / this.threadRam);
                this.addNewServer(server.name, nThreads);
            }
        }
    }

    public addNewServer(server: string, maxThreads: number): void {
        if (server in Object.keys(this.pool)) {
            throw new Error(`Server ${server} already in pool`);
        }
        this.pool[server] = {
            maxThreads,
            hackThreads: 0,
            growThreads: 0,
            weakenThreads: 0,
        };
    }

    public updateServerThreads(server: string, newThreads: number): void {
        if (!(server in Object.keys(this.pool))) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].maxThreads = newThreads;
    }

    public size(): number {
        return Object.values(this.pool).reduce((acc, server) => acc + server.maxThreads, 0);
    }

    public available(): number {
        return Object.values(this.pool).reduce((acc, server) => acc + server.maxThreads - server.hackThreads - server.growThreads - server.weakenThreads, 0);
    }

    public headCount(): ThreadInfo {
        return Object.values(this.pool).reduce((acc, server) => {
            acc.maxThreads += server.maxThreads;
            acc.hackThreads += server.hackThreads;
            acc.growThreads += server.growThreads;
            acc.weakenThreads += server.weakenThreads;
            return acc;
        }, { maxThreads: 0, hackThreads: 0, growThreads: 0, weakenThreads: 0 });
    }
}

export let pool: ThreadPool;

export async function main(ns: NS): Promise<void> {
    let graph = getGraph(ns);
    for (const serverInfo of graph) {
        if (serverInfo.name === "home") {
            ns.scriptKill("hack.js", "home");
            ns.scriptKill("grow.js", "home");
            ns.scriptKill("weaken.js", "home");
            continue;
        }
        if (ns.hasRootAccess(serverInfo.name)) {
            ns.killall(serverInfo.name, true);
        }
    }
    ns.disableLog("getServerUsedRam");
    ns.disableLog("getServerMaxRam");

    function setTarget(): string {
        let bestFit = "n00dles";
        for (const servInfo of graph) {
            if (canHack(ns, servInfo.name)) {
                if (isProfitable(ns, servInfo.name)) {
                    bestFit = servInfo.name;
                } else {
                    continue;
                }
            } else {
                break;
            }
        }
        return bestFit;
    }

    const target = (ns.args[0] as string ?? setTarget());

    if (pool === undefined) {
        pool = new ThreadPool(ns);
    }
    if (!pool.isInitialized) {
        pool.init(ns, graph);
        ns.print(`Initialized pool with ${pool.size()} threads across ${Object.keys(pool.pool).length} servers`);
    }

    for (; ;) {
        const headCount = pool.headCount();
        ns.print(`Threads: ${headCount.hackThreads} hack, ${headCount.growThreads} grow, ${headCount.weakenThreads} weaken`);
        const available = headCount.maxThreads - (headCount.hackThreads + headCount.growThreads + headCount.weakenThreads);
        ns.print(`Available: ${available}`);

        let newHack = Math.floor((available * hgw.hack) / (hgw.hack + hgw.grow + hgw.weaken));
        let newGrow = Math.floor((available * hgw.grow) / (hgw.hack + hgw.grow + hgw.weaken));
        let newWeaken = available - newHack - newGrow;
        ns.print(`Spawning ${newHack} new hacking threads across network...`);


        ns.print(`Spawning ${newGrow} new grow threads across network...`);
        for (const server of graph.map((node) => node.name)) {
            if (newGrow <= 0) break;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.pool[server];
            let newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
            if (newServerThreads > newGrow) {
                newServerThreads = newGrow;
            } else if (newServerThreads <= 0) {
                continue;
            }
            if (!ns.fileExists("grow.js", server)) {
                if (server === "home") {
                    ns.print("Failed to find grow.js in home");
                    break;
                }
                const res = ns.scp("grow.js", server, "home");
                if (!res) {
                    ns.print(`Failed to scp grow.js to ${server}`);
                    break;
                }
            }
            ns.exec("grow.js", server, { threads: newServerThreads }, target);
            pool.pool[server].growThreads += newServerThreads;
            newGrow -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} grow threads on ${server}, ${newGrow} remaining`);
        }

        ns.print(`Spawning ${newWeaken} new weaken threads across network...`);
        for (const server of graph.map((node) => node.name)) {
            if (newWeaken <= 0) break;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.pool[server];
            let newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
            if (newServerThreads > newWeaken) {
                newServerThreads = newWeaken;
            } else if (newServerThreads <= 0) {
                continue;
            }
            if (!ns.fileExists("weaken.js", server)) {
                if (server === "home") {
                    ns.print("Failed to find weaken.js in home");
                    break;
                }
                const res = ns.scp("weaken.js", server, "home");
                if (!res) {
                    ns.print(`Failed to scp weaken.js to ${server}`);
                    break;
                }
            }
            ns.exec("weaken.js", server, { threads: newServerThreads }, target);
            pool.pool[server].weakenThreads += newServerThreads;
            newWeaken -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} weaken threads on ${server}, ${newWeaken} remaining`);
        }
        for (const server of graph.map((node) => node.name)) {
            if (newHack <= 0) break;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.pool[server];
            let newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
            if (newServerThreads > newHack) {
                newServerThreads = newHack;
            } else if (newServerThreads <= 0) {
                continue;
            }
            if (!ns.fileExists("hack.js", server)) {
                if (server === "home") {
                    ns.print("Failed to find hack.js in home");
                    break;
                }
                const res = ns.scp("hack.js", server, "home");
                if (!res) {
                    ns.print(`Failed to scp hack.js to ${server}`);
                    break;
                }
            }
            ns.exec("hack.js", server, { threads: newServerThreads }, target);
            pool.pool[server].hackThreads += newServerThreads;
            newHack -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} hack threads on ${server}, ${newHack} remaining`);
            if (newHack > 0) await ns.asleep(1000);
        }
        await ns.sleep(1000);
        graph = getGraph(ns);
        pool.update(ns, graph);
    }
}