import { AutocompleteData, NS, Player, Server } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";
import { rip } from "./rip";

const homeReserve = 64;
const batchSize = 10_000;
const defaultHGW = { hack: 1, grow: 10, weaken: 2 };
// const defaultHGW = { hack: 1, grow: 0, weaken: 0 };

type HGWOutcome = {
    timeElapsed: number,
    moneyYield: number,
    deltaExp: number,
    deltaSec: number,
    serverMoneyPercent: number,
}

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

async function stagger(ns: NS) {
    await ns.sleep(100);
    return;
}

function balanceHGW(ns: NS, server: string, pool: ThreadPool, hgw: HGWRatio): HGWRatio {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const iterLimit = 100;
    const poolSize = pool.size();

    const plyrObj = ns.getPlayer();
    const servObj = ns.getServer(server);

    /*
    function _growTime(tmpServ: Server, tmpPlyr: Player): number {
        return ns.formulas.hacking.growTime(tmpServ, tmpPlyr);
    }
    function _hackTime(tmpServ: Server, tmpPlyr: Player): number {
        return ns.formulas.hacking.hackTime(tmpServ, tmpPlyr);
    }
    function _weakenTime(tmpServ: Server, tmpPlyr: Player): number {
        return ns.formulas.hacking.weakenTime(tmpServ, tmpPlyr);
    }

    const maxMoney = servObj.moneyMax ?? 0;
    const initMoney = Math.max(1, servObj.moneyAvailable ?? 0);

    const homeCores = ns.getServer("home").cpuCores ?? 1;
    const homeMaxThreads = pool.getInfo("home").maxThreads ?? 0;

    const minSec = servObj.minDifficulty ?? 1;
    const initSec = servObj.hackDifficulty ?? minSec;

    function _evHackDilationFactor(tmpServ: Server, tmpPlyr: Player,  nThreads: number): number {
        const successRate = ns.formulas.hacking.hackChance(tmpServ, tmpPlyr);
        const hackYield = ns.formulas.hacking.hackPercent(tmpServ, tmpPlyr);
        return Math.max(0, (1 - (successRate * hackYield * nThreads)));
    }

    let iterSec = initSec;
    let iterMoney = initMoney;
    let iterServ = { ...servObj };

    const hackThreadsBounds = { minimum: 1, maximum: poolSize };
    let nHack = Math.ceil(poolSize * hackFraction(hgw));

    function getOutcome(simServ: Server, simPlyr: Player, tithe: number): HGWOutcome {
        let iterServ = { ...simServ };
        let iterPlyr = { ...simPlyr };
        let hackCountdown = _hackTime(iterServ, iterPlyr);
        let growCountdown = _growTime(iterServ, iterPlyr);
        let weakenCountdown = _weakenTime(iterServ, iterPlyr);

        function hackThreadsForAmount(serv: Server, plyr: Player, hackAmount: number) {
            if (hackAmount > serv.moneyAvailable! || hackAmount <= 0) {
                return 0;
            }
            const bounds = { minimum: 1, maximum: poolSize };
            let nHack = Math.ceil(poolSize * hackFraction(hgw));
            do {
                const moneyYield = serv.moneyAvailable! * _evHackDilationFactor(serv, plyr, nHack);
                if (moneyYield > hackAmount) {
                    bounds.maximum = nHack;
                } else {
                    bounds.minimum = nHack;
                }
                nHack = Math.ceil((bounds.minimum + bounds.maximum) / 2);
            } while (bounds.minimum + 1 < bounds.maximum);
            return nHack;
        }

        function growThreadsForMax(serv: Server, plyr: Player): [number, number] {
            const homeMaxThreads = pool.getInfo("home").maxThreads ?? 0;
            const homeCores = ns.getServer("home").cpuCores ?? 1;
            const idealHomeThreads = ns.formulas.hacking.growThreads(serv, plyr, serv.moneyMax!, homeCores);
            if (idealHomeThreads <= homeMaxThreads) {
                return [idealHomeThreads, 0];
            } else {
                let homeContribution = ns.formulas.hacking.growPercent(serv, homeMaxThreads, plyr, homeCores);
                return [homeMaxThreads, ns.formulas.hacking.growThreads({...serv, moneyAvailable: (serv.moneyAvailable! * homeContribution) }, plyr, serv.moneyMax!)];
            }
        }

        function getWeakenThreadsForMin(serv: Server, plyr: Player) {
            const bounds = { minimum: 1, maximum: poolSize };
            let nWeaken = Math.ceil(poolSize * weakenFraction(hgw));
            let idealYield = serv.hackDifficulty! - serv.minDifficulty!;
            do {
                const secYield = ns.weakenAnalyze(nWeaken);
                if (secYield < idealYield) {
                    bounds.maximum = nWeaken;
                } else if (secYield > idealYield) {
                    bounds.minimum = nWeaken;
                }
                nWeaken = Math.ceil((bounds.minimum + bounds.maximum) / 2);
            } while (bounds.minimum + 1 < bounds.maximum);
            return nWeaken;
        }

        let ticks = 0;

        let nHack = 1;
        let nGrow = 1;
        let nWeaken = 1;

        let scale = 1;

        let timesHacked = 0;
        let timesWeakened = 0;
        let timesGrown = 0;

        for ( ; timesHacked * timesGrown * timesWeakened == 0; ticks++) {
            if (nHack + nWeaken + nGrow > poolSize) {
                scale = poolSize / (nHack + nWeaken + nGrow);
            }
            if (hackCountdown <= 0) {
                nHack = hackThreadsForAmount(iterServ, iterPlyr, tithe * iterServ.moneyAvailable!);
                const actualHack = nHack * scale;
                const dilation = _evHackDilationFactor(iterServ, iterPlyr, actualHack);
                iterPlyr.money += iterServ.moneyAvailable! * (1 - dilation);
                iterPlyr.exp.hacking += ns.formulas.hacking.hackExp(iterServ, iterPlyr) * nHack;
                iterServ.moneyAvailable! *= dilation;
                iterServ.hackDifficulty! += ns.hackAnalyzeSecurity(actualHack, server);
                timesHacked++;
                hackCountdown = _hackTime(iterServ, iterPlyr);
            } else {
                hackCountdown--;
            }

            if (growCountdown <= 0) {
                const [hGrow, sGrow] = growThreadsForMax(iterServ, iterPlyr);
                nGrow = hGrow + sGrow;
                const [actualHGrow, actualSGrow] = [hGrow, (nGrow * scale) - hGrow];
                iterServ.moneyAvailable! *= ns.formulas.hacking.growPercent(iterServ, actualHGrow, iterPlyr, homeCores)
                iterServ.hackDifficulty! += ns.growthAnalyzeSecurity(actualHGrow, server, homeCores);
                iterServ.moneyAvailable! *= ns.formulas.hacking.growPercent(iterServ, actualSGrow, iterPlyr);
                iterServ.hackDifficulty! += ns.growthAnalyzeSecurity(actualHGrow, server);
                timesGrown++;
                growCountdown = _growTime(iterServ, iterPlyr);
            } else {
                growCountdown--;
            }

            if (weakenCountdown <= 0) {
                let nWeaken = getWeakenThreadsForMin(iterServ, iterPlyr);
                let actualWeaken = nWeaken * scale;
                iterServ.hackDifficulty! -= ns.weakenAnalyze(actualWeaken);
                timesWeakened++;
                weakenCountdown = _weakenTime(iterServ, iterPlyr);
            } else {
                weakenCountdown--;
            }
        }
        const ret: HGWOutcome =
        {
            timeElapsed: ticks,
            deltaExp: iterPlyr.exp.hacking - simPlyr.exp.hacking,
            moneyYield: iterPlyr.money - simPlyr.money,
            deltaSec: iterServ.hackDifficulty! - simServ.hackDifficulty!,
            serverMoneyPercent: iterServ.moneyAvailable! / iterServ.moneyMax!,
        };
        return ret;
    }

    for (let i = 0; i < iterLimit; i++) {
        for (let j = 0; j < _hacksPerWeaken(iterSec); j++) {
            iterMoney *= _evHackDilationFactor(iterSec, iterMoney, nHack);
            iterSec += ns.hackAnalyzeSecurity(nHack, server);
            if (iterMoney <= 1) {
                break;
            }
        }
        if (iterMoney <= 1) {
            // Hack is too strong compared to grow and weaken
            hackThreadsBounds.maximum = nHack;
            hgw.grow *= 2;
            hgw.weaken *= 2;
            continue;
        }

        if (iterServ.hackDifficulty !== undefined) {
            iterServ.hackDifficulty = iterSec;
        }
        if (iterServ.moneyAvailable !== undefined) {
            iterServ.moneyAvailable = iterMoney;
        }

        const homePart = (iterServ.moneyAvailable ?? 1) * ns.formulas.hacking.growPercent(iterServ,idealHomeGrowThreads, plyrObj, homeCores);
        const idealAwayGrowThreads = ns.formulas.hacking.growThreads({ ...iterServ, moneyAvailable: homePart }, plyrObj, maxMoney, 1);
        const idealTotalGrowThreads = idealHomeGrowThreads + idealAwayGrowThreads;

        (nGrow > idealTotalGrowThreads) {
            // Grow is too strong compared to hack and weaken
            hgw.hack *= 2;
            hgw.weaken *= 2;
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

async function getHGW(ns: NS, server: string, pool: ThreadPool): Promise<HGWRatio> {
    if (hasFormulas(ns)) {
        const hgw = defaultHGW;
        // TODO - use formulas API to calculate ideal HGW
        return balanceHGW(ns, server, pool, hgw);
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
                    const maxRam = ns.getServerMaxRam(server.name);
                    const usedRam = ns.getServerUsedRam(server.name);
                    const freeRam = Math.max(((server.name === "home") ? maxRam - homeReserve : maxRam) - usedRam, 0);
                    const nThreads = Math.floor(freeRam / this.threadRam);
                    this.addNewServer(server.name, nThreads);
                } else {
                    this.addNewServer(server.name, 0);
                }
            } else {
                let maxRam = ns.getServerMaxRam(server.name);
                if (server.name == "home") {
                    maxRam = Math.max(maxRam - homeReserve, 0);
                }
                if (maxRam > this.pool[server.name].maxThreads * this.threadRam) {
                    ns.tprint(`INFO: ${server.name} max RAM increased to ${ns.formatRam(maxRam)}`);
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
    const hgw = await getHGW(ns, target, pool);

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
                    await stagger(ns);
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
                    await stagger(ns);
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
                    await stagger(ns);
                }
                ns.print(`Spawned ${newServerThreads} hack threads on ${server}, ${newHack} remaining`);
            }
        }

        await ns.sleep(1000);
        graph = getGraph(ns);
        const currentSec = ns.getServerSecurityLevel(target);
        const currentMoney = ns.getServerMoneyAvailable(target);
        ns.clearLog();
        ns.print(`${target} Snapshot: Security = ${currentSec.toFixed(2)} (Min: ${minSec}), Money = $${ns.formatNumber(currentMoney)}`);
        pool.update(ns, graph);
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]): string[] {
    return [...data.servers]
}
