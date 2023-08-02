import { AutocompleteData, NS } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";
import { rip } from "./rip";

const homeReserve = 64;
const defaultHGW = { hack: 1, grow: 10, weaken: 2 };


function getHGW(ns: NS, server: string): { hack: number, grow: number, weaken: number } {

    let money = ns.getServerMoneyAvailable(server);
    if (money === 0) money = 1;
    const maxMoney = ns.getServerMaxMoney(server);
    const minSec = ns.getServerMinSecurityLevel(server);
    const sec = ns.getServerSecurityLevel(server);

    const hack = Math.ceil(ns.hackAnalyzeThreads(server, money));
    const grow = Math.ceil(ns.growthAnalyze(server, maxMoney / money));
    const weaken = Math.ceil((sec - minSec) * 20);
    if (hack < weaken && weaken < grow) {
        return { hack, grow, weaken };
    } else {
        return defaultHGW;
    }
}

type ThreadInfo = { maxThreads: number, hackThreads: number, growThreads: number, weakenThreads: number };

export class ThreadPool {
    public isInitialized = false;
    protected pool: { [server: string]: ThreadInfo };
    protected readonly threadRam;

    constructor(ns: NS) {
        this.pool = {};
        this.threadRam = Math.max(ns.getScriptRam("hack.js", "home"), ns.getScriptRam("grow.js", "home"), ns.getScriptRam("weaken.js", "home"));
    }

    public knownServers(): string[] {
        return Object.keys(this.pool);
    }

    public init(ns: NS, graph: NodeInfo[]): void {
        if (this.isInitialized) return;
        for (const server of graph) {
            if (!ns.serverExists(server.name)) {
                continue;
            }
            if (ns.hasRootAccess(server.name)) {
                let freeRam = ns.getServerMaxRam(server.name) - ns.getServerUsedRam(server.name);
                if (server.name == "home") {
                    freeRam = Math.max(freeRam - homeReserve, 0);
                }
                const nThreads = Math.floor(freeRam / this.threadRam);
                this.addNewServer(server.name, nThreads);
            } else {
                this.addNewServer(server.name, 0);
            }
        }
        this.isInitialized = true;
    }

    public update(ns: NS, graph: NodeInfo[]): void {
        for (const server of graph) {
            if (!(server.name in Object.keys(this.pool))) {
                if (!ns.serverExists(server.name)) {
                    continue;
                }
                if (ns.hasRootAccess(server.name)) {
                    let freeRam = ns.getServerMaxRam(server.name) - ns.getServerUsedRam(server.name);
                    if (server.name == "home") {
                        freeRam = Math.max(freeRam - homeReserve, 0);
                    }
                    const nThreads = Math.floor(freeRam / this.threadRam);
                    this.addNewServer(server.name, nThreads);
                } else {
                    this.addNewServer(server.name, 0);
                }
            }
        }
    }

    public addHackThreads(server: string, nThreads: number): void {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].hackThreads += nThreads;
    }

    public addGrowThreads(server: string, nThreads: number): void {
        if (!(Object.keys(this.pool).includes(server))) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].growThreads += nThreads;
    }

    getInfo(server: string) {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        return this.pool[server];
    }

    public addWeakenThreads(server: string, nThreads: number): void {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].weakenThreads += nThreads;
    }

    public addNewServer(server: string, nThreads: number): void {
        this.pool[server] = {
            maxThreads: Math.max(this.pool[server]?.maxThreads ?? 0, nThreads),
            hackThreads: this.pool[server]?.hackThreads ?? 0,
            growThreads: this.pool[server]?.growThreads ?? 0,
            weakenThreads: this.pool[server]?.weakenThreads ?? 0,
        };
    }

    public updateServerThreads(server: string, newThreads: number): void {
        if (!(Object.keys(this.pool).includes(server))) {
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


export async function main(ns: NS): Promise<void> {
    let graph = getGraph(ns);
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
    ns.disableLog("getServerUsedRam");
    ns.disableLog("getServerMaxRam");
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("getServerMaxMoney");
    ns.disableLog("getServerMinSecurityLevel");
    ns.disableLog("getServerSecurityLevel");
    ns.disableLog("sleep");

    function setTarget(): string {
        const highestProfit = rip(ns);
        for (const candidate of highestProfit) {
            if (canHack(ns, candidate[0])) {
                return candidate[0];
            } else {
                continue;
            }
        }
        return "n00dles";
    }

    const target = (ns.args[0] as string ?? setTarget());
    const minSec = ns.getServerMinSecurityLevel(target);

    const pool = new ThreadPool(ns);
    pool.init(ns, graph);
    ns.tprint(`Initialized pool with ${pool.size()} threads across ${pool.knownServers().length} servers`);

    for (; ;) {
        const headCount = pool.headCount();
        ns.print(`Threads: ${headCount.hackThreads} hack, ${headCount.growThreads} grow, ${headCount.weakenThreads} weaken`);
        const available = headCount.maxThreads - (headCount.hackThreads + headCount.growThreads + headCount.weakenThreads);
        ns.print(`Available: ${available}`);

        const hgw = getHGW(ns, target);
        let newHack = Math.ceil((available * hgw.hack) / (hgw.hack + hgw.grow + hgw.weaken));
        let newGrow = Math.floor((available * hgw.grow) / (hgw.hack + hgw.grow + hgw.weaken));
        let newWeaken = available - newHack - newGrow;

        if (newGrow > 0) ns.print(`Spawning ${newGrow} new grow threads across network...`);
        for (const server of graph.map((node) => node.name)) {
            if (newGrow <= 0) break;
            if (!ns.serverExists(server)) continue;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.getInfo(server);
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
            if (ns.exec("grow.js", server, { threads: newServerThreads }, target) === 0) continue;
            pool.addGrowThreads(server, newServerThreads);
            newGrow -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} grow threads on ${server}, ${newGrow} remaining`);
        }

        if (newWeaken > 0) ns.print(`Spawning ${newWeaken} new weaken threads across network...`);
        for (const server of graph.map((node) => node.name)) {
            if (newWeaken <= 0) break;
            if (!ns.serverExists(server)) continue;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.getInfo(server);
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
            if (ns.exec("weaken.js", server, { threads: newServerThreads }, target) === 0) continue;
            pool.addWeakenThreads(server, newServerThreads);
            newWeaken -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} weaken threads on ${server}, ${newWeaken} remaining`);
        }

        if (newHack > 0) ns.print(`Spawning ${newHack} new hacking threads across network...`);
        for (const server of graph.map((node) => node.name)) {
            if (newHack <= 0) {
                break;
            }
            if (!ns.serverExists(server)) continue;
            if (!ns.hasRootAccess(server)) continue;
            const info = pool.getInfo(server);
            let newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
            if (newServerThreads > newHack) {
                newServerThreads = newHack;
            } else if (newServerThreads <= 0) {
                continue;
            }
            if (server !== "home") {
                const res = ns.scp("hack.js", server, "home");
                if (!res) {
                    ns.print(`Failed to scp hack.js to ${server}`);
                }
            }
            if (ns.exec("hack.js", server, { threads: newServerThreads }, target) === 0) continue;
            pool.addHackThreads(server, newServerThreads);
            newHack -= newServerThreads;
            ns.print(`Spawned ${newServerThreads} hack threads on ${server}, ${newHack} remaining`);
        }

        await ns.sleep(1000);
        graph = getGraph(ns);
        const currentSec = ns.getServerSecurityLevel(target);
        const currentMoney = ns.getServerMoneyAvailable(target);
        ns.clearLog();
        ns.print(`Server Snapshot: Security = ${currentSec} (Min: ${minSec}), Money = $${ns.formatNumber(currentMoney)}`);
        pool.update(ns, graph);
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]): string[] {
    return [...data.servers]
}