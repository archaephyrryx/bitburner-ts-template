import { AutocompleteData, NS } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";
import { rip } from "./rip";

const homeReserve = 16; //64;
const batchSize = 100000;
const defaultHGW = { hack: 1, grow: 20, weaken: 2 };

function hackFraction(hgw: HGWRatio): number {
    return hgw.hack / (hgw.hack + hgw.grow + hgw.weaken);
}

function growFraction(hgw: HGWRatio): number {
    return hgw.grow / (hgw.hack + hgw.grow + hgw.weaken);
}

function weakenFraction(hgw: HGWRatio): number {
    return hgw.weaken / (hgw.hack + hgw.grow + hgw.weaken);
}

function hasFormulas(ns: NS) {
    return ns.fileExists("Formulas.exe", "home");
}

type HGWRatio = { hack: number, grow: number, weaken: number }

function balanceHGW(ns: NS, server: string, poolSize: number, hgw: HGWRatio): HGWRatio {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const limit = 100;

    const growTime = ns.getGrowTime(server);
    const hackTime = ns.getHackTime(server);
    const weakenTime = ns.getWeakenTime(server);

    const hacksPerGrow = growTime / hackTime;
    const hacksPerWeaken = weakenTime / hackTime;
    const growsPerWeaken = growTime / weakenTime;

    const maxMoney = ns.getServerMaxMoney(server);
    const initMoney = Math.max(1, ns.getServerMoneyAvailable(server));
    const minSec = ns.getServerMinSecurityLevel(server);
    const initSec = ns.getServerSecurityLevel(server);

    function inverseGrow(nGrow: number, factor: number) {
        let minFactor = 1.0;
        let maxFactor = factor;

        let sampleFactor = Math.exp((Math.log(minFactor) + Math.log(maxFactor)) / 2);
        for (let iter = 0; iter < 100; iter++) {
            const reqThreads = Math.ceil(ns.growthAnalyze(server, sampleFactor));
            if (reqThreads > nGrow) {
                // nGrow's factor must be lower than sampleFactor
                maxFactor = sampleFactor;
            } else if (reqThreads < nGrow) {
                // nGrow's factor must be higher than sampleFactor
                minFactor = sampleFactor;
            } else {
                break;
            }
            sampleFactor = Math.exp((Math.log(minFactor) + Math.log(maxFactor)) / 2);
        }
        return sampleFactor;
    }

    /*
    for (let i = 0; i < limit; i++) {

        const nHack = Math.ceil(poolSize * hackFraction(hgw));
        const nGrow = Math.ceil(poolSize * growFraction(hgw));
        const nWeaken = Math.ceil(poolSize * weakenFraction(hgw));

        const hackDilation = (1 - (ns.hackAnalyze(server) * nHack));
        if (hackDilation <= 0) {
            // Hack is too strong compared to grow and weaken
            hgw.weaken *= 2;
            hgw.grow *= 2;
            continue;
        }


        const reductionPerGrowCycle = (1 - Math.pow(

            const growFactor = inverseGrow(nGrow, maxMoney / moneyAfter);

        if (growFactor / < )
            const y = ns.growthAnalyzeSecurity(nGrow, server);



        const secIncrease = Math.ceil(ns.hackAnalyzeSecurity(nHack, server));
        const secAfter = initSec + secIncrease;
        const weakening = Math.ceil(ns.weakenAnalyze(nWeaken));
        if (weakening > secAfter - minSec) {
            // Weaken is too strong
            hgw.weaken *= 0.8;
        } else if (weakening < secIncrease) {
            // Weaken is too weak
            hgw.weaken *= 2;
        }



        const grow = Math.ceil(ns.growthAnalyzePercent(server, maxMoney / initMoney));
        const weaken = Math.ceil(ns.weakenAnalyzePercent(server, initSec - minSec));
        if (hack < weaken && weaken < grow) {
            hgw = { hack, grow, weaken };
            break;
        }
    }
    */
    return hgw;
}

async function getHGW(ns: NS, server: string, poolSize: number): Promise<HGWRatio> {
    if (hasFormulas(ns)) {
        const hgw = defaultHGW;
        // TODO - use formulas API to calculate ideal HGW
        return balanceHGW(ns, server, poolSize, hgw);
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
            } else {
                let maxRam = ns.getServerMaxRam(server.name);
                if (server.name == "home") {
                    maxRam = Math.max(maxRam - homeReserve, this.pool[server.name].maxThreads * this.threadRam);
                }
                const maxThreads = Math.floor(maxRam / this.threadRam);
                this.updateServerThreads(server.name, maxThreads);
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
    ns.write("target.txt", target, "w");
    ns.write("dispatch-pid.txt", ns.getRunningScript()?.pid.toString() ?? "0", "w");
    ns.tail();

    const minSec = ns.getServerMinSecurityLevel(target);

    const pool = new ThreadPool(ns);
    pool.init(ns, graph);
    ns.tprint(`Initialized pool with ${pool.size()} threads across ${pool.knownServers().length} servers`);
    const size = pool.size();
    const hgw = await getHGW(ns, target, size);

    for (; ;) {
        const headCount = pool.headCount();
        ns.print(`Threads: ${headCount.hackThreads} hack, ${headCount.growThreads} grow, ${headCount.weakenThreads} weaken`);
        const available = headCount.maxThreads - (headCount.hackThreads + headCount.growThreads + headCount.weakenThreads);
        ns.print(`Available: ${available}`);

        let newHack = Math.ceil(available * hackFraction(hgw));
        let newGrow = Math.floor(available * growFraction(hgw));
        let newWeaken = available - newHack - newGrow;

        if (newHack + newGrow + newWeaken > 0) {
            ns.print(`New threads: ${newHack} hack, ${newGrow} grow, ${newWeaken} weaken`);
            if (newGrow > 0) ns.print(`Spawning ${newGrow} new grow threads across network...`);
            outer: for (const server of graph.map((node) => node.name)) {
                if (newGrow <= 0) break;
                if (!ns.serverExists(server)) continue;
                if (!ns.hasRootAccess(server)) continue;
                const info = pool.getInfo(server);
                const newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
                let remainingThreads = newServerThreads;
                while (remainingThreads > 0 && newGrow > 0) {
                    const nGrow = Math.min(newGrow, batchSize);
                    let threads = remainingThreads;
                    if (threads > nGrow) {
                        threads = nGrow;
                    } else if (threads <= 0) {
                        continue outer;
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
                    if (ns.exec("grow.js", server, { threads }, target) === 0) continue;
                    pool.addGrowThreads(server, threads);
                    newGrow -= threads;
                    remainingThreads -= threads;
                }
                ns.print(`Spawned ${newServerThreads} grow threads on ${server}, ${newGrow} remaining`);
            }

            if (newWeaken > 0) ns.print(`Spawning ${newWeaken} new weaken threads across network...`);
            outer: for (const server of graph.map((node) => node.name)) {
                if (newWeaken <= 0) break;
                if (!ns.serverExists(server)) continue;
                if (!ns.hasRootAccess(server)) continue;
                const info = pool.getInfo(server);
                const newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
                let remainingThreads = newServerThreads;
                while (remainingThreads > 0 && newWeaken > 0) {
                    let threads = remainingThreads;
                    const nWeaken = Math.min(newWeaken, batchSize);
                    if (threads > nWeaken) {
                        threads = nWeaken;
                    } else if (threads <= 0) {
                        continue outer;
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
                    if (ns.exec("weaken.js", server, { threads }, target) === 0) continue;
                    pool.addWeakenThreads(server, threads);
                    newWeaken -= threads;
                    remainingThreads -= threads;
                }
                ns.print(`Spawned ${newServerThreads} weaken threads on ${server}, ${newWeaken} remaining`);
            }

            if (newHack > 0) ns.print(`Spawning ${newHack} new hacking threads across network...`);
            outer: for (const server of graph.map((node) => node.name)) {
                if (newHack <= 0) {
                    break;
                }
                if (!ns.serverExists(server)) continue;
                if (!ns.hasRootAccess(server)) continue;
                const info = pool.getInfo(server);
                const newServerThreads = info.maxThreads - (info.hackThreads + info.growThreads + info.weakenThreads);
                let remainingThreads = newServerThreads;
                while (remainingThreads > 0 && newHack > 0) {
                    const nHack = Math.min(newHack, batchSize);
                    let threads = remainingThreads;
                    if (threads > nHack) {
                        threads = nHack;
                    } else if (threads <= 0) {
                        continue outer;
                    }
                    if (server !== "home") {
                        const res = ns.scp("hack.js", server, "home");
                        if (!res) {
                            ns.print(`Failed to scp hack.js to ${server}`);
                        }
                    }
                    if (ns.exec("hack.js", server, { threads }, target) === 0) continue;
                    pool.addHackThreads(server, threads);
                    newHack -= threads;
                    remainingThreads -= threads;
                }
                ns.print(`Spawned ${newServerThreads} hack threads on ${server}, ${newHack} remaining`);
            }
        }

        await ns.sleep(1000);
        graph = getGraph(ns);
        const currentSec = ns.getServerSecurityLevel(target);
        const currentMoney = ns.getServerMoneyAvailable(target);
        ns.clearLog();
        ns.print(`${target} Snapshot: Security = ${currentSec} (Min: ${minSec}), Money = $${ns.formatNumber(currentMoney)}`);
        pool.update(ns, graph);
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]): string[] {
    return [...data.servers]
}