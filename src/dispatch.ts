import { AutocompleteData, NS } from "@ns";
import { getGraph } from "census";
import { NodeInfo } from "global";
import { canHack } from "./helper";
import { rip } from "./rip";

type Target = { server: string, stock: boolean };

const MaxNonStockTargets = 5;

const homeReserve = 64;
const batchSize = 100_000;

const defaultHGW = { hack: 1, grow: 8, weaken: 2 };

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
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const iterLimit = 100;
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const poolSize = pool.size();

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const plyrObj = ns.getPlayer();
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
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
    const hgw = defaultHGW;
    // TODO - use formulas API to calculate ideal HGW
    return balanceHGW(ns, server, pool, hgw);
}



class ProcessInfo {
    protected pidThreads: { [pid: number]: number };

    constructor() {
        this.pidThreads = {};
    }

    public addProcess(pid: number, threads: number) {
        if (pid in this.pidThreads) {
            throw new Error("Duplicate PID detected: " + pid);
        }
        this.pidThreads[pid] = threads;
    }

    public getThreadCount(pid: number): number {
        return this.pidThreads[pid] ?? 0;
    }

    /**
     * Removes a process identifier and returns how many threads it had, if any.
     * @param pid
     */
    public pruneProcess(pid: number): number {
        const ret = this.pidThreads[pid] ?? 0;
        delete this.pidThreads[pid];
        return ret;
    }

    get pids(): number[] {
        const ret = [];
        for (const key in this.pidThreads) {
            ret.push(Number(key));
        }
        return ret;
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
                this.updateServerThreads(ns, server.name, maxThreads);
            }
        }
    }

    public addHackThreads(server: string, nThreads: number, pid?: number): void {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].hackThreads += nThreads;
        if (pid) {
            this.pool[server].grows.addProcess(pid, nThreads);
        }
    }

    public addGrowThreads(server: string, nThreads: number, pid?: number): void {
        if (!(Object.keys(this.pool).includes(server))) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].growThreads += nThreads;
        if (pid) {
            this.pool[server].grows.addProcess(pid, nThreads);
        }
    }

    public addWeakenThreads(server: string, nThreads: number, pid?: number): void {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].weakenThreads += nThreads;
        if (pid) {
            this.pool[server].weakens.addProcess(pid, nThreads);
        }
    }

    getInfo(server: string) {
        if (!Object.keys(this.pool).includes(server)) {
            throw new Error(`Server ${server} not in pool`);
        }
        return this.pool[server];
    }

    public addNewServer(server: string, nThreads: number): void {
        this.pool[server] = {
            maxThreads: Math.max(this.pool[server]?.maxThreads ?? 0, nThreads),
            hackThreads: this.pool[server]?.hackThreads ?? 0,
            growThreads: this.pool[server]?.growThreads ?? 0,
            weakenThreads: this.pool[server]?.weakenThreads ?? 0,
            hacks: new ProcessInfo(),
            weakens: new ProcessInfo(),
            grows: new ProcessInfo(),
        };
    }

    public updateServerThreads(ns: NS, server: string, newThreads: number): void {
        if (!(Object.keys(this.pool).includes(server))) {
            throw new Error(`Server ${server} not in pool`);
        }
        this.pool[server].maxThreads = newThreads;
        const hackPids = this.pool[server].hacks.pids;
        const growPids = this.pool[server].grows.pids;
        const weakenPids = this.pool[server].weakens.pids;
        for (const pid of hackPids) {
            if (!ns.isRunning(pid)) {
                this.pool[server].hackThreads -= this.pool[server].hacks.pruneProcess(pid);
            }
        }
        for (const pid of growPids) {
            if (!ns.isRunning(pid)) {
                this.pool[server].growThreads -= this.pool[server].grows.pruneProcess(pid);
            }
        }
        for (const pid of weakenPids) {
            if (!ns.isRunning(pid)) {
                this.pool[server].weakenThreads -= this.pool[server].weakens.pruneProcess(pid);
            }
        }
    }

    public size(): number {
        return Object.values(this.pool).reduce((acc, server) => acc + server.maxThreads, 0);
    }

    public available(): number {
        return Object.values(this.pool).reduce((acc, server) => acc + server.maxThreads - server.hackThreads - server.growThreads - server.weakenThreads, 0);
    }

    public headCount(): ThreadSummary {
        return Object.values(this.pool).reduce((acc, server) => {
            acc.maxThreads += server.maxThreads;
            acc.hackThreads += server.hackThreads;
            acc.growThreads += server.growThreads;
            acc.weakenThreads += server.weakenThreads;
            return acc;
        }, { maxThreads: 0, hackThreads: 0, growThreads: 0, weakenThreads: 0 });
    }
}

function renew(ns: NS): NodeInfo[] {
    const graph = getGraph(ns);
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

function decideTarget(ns: NS): [string, boolean] {
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
    const cmdLineTarget = ns.args[0];
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

async function spawnTarget(ns: NS, target: string, graph: NodeInfo[], userPicked = false): Promise<string | -1> {
    ns.write("target.txt", target, "w");
    ns.write("dispatch-pid.txt", ns.pid.toString() ?? "0", "w");

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
        graph = getGraph(ns);
        const currentSec = ns.getServerSecurityLevel(target);
        const currentMoney = ns.getServerMoneyAvailable(target);
        ns.clearLog();
        ns.print(`${target} Snapshot: Security = ${currentSec.toFixed(2)} (Min: ${minSec}), Money = $${ns.formatNumber(currentMoney)}`);
        pool.update(ns, graph);
        if (!userPicked) {
            const [nextTarget] = decideTarget(ns);
            if (nextTarget !== target) {
                return nextTarget;
            }
        }
    }
}


export async function main(ns: NS): Promise<void> {
    hideLogs(ns);
    const graph = renew(ns);
    const [initTarget, userPicked] = decideTarget(ns);
    ns.tail();

    let target = initTarget;
    for (; ;) {
        const newTarget = await spawnTarget(ns, target, graph, userPicked);
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
