import { NS, HacknetNodeConstants } from "@ns";
import { BUDGET, makePurchase } from './budget';
import { printWaitingMoney } from "./helper";
import { formatTime } from "./helper";

export const MAX_RAM = 64;
export const MAX_LEVEL = 200;
export const MAX_CORES = 16;
export const MAXAMOUNT = (ns: NS) => ns.hacknet.maxNumNodes();

const MONEY_WAIT_SLEEP_MS = 3000;

interface NSHelper {
    ns: NS,
    moneyAvailable: number,
}

function getHelper(ns: NS): NSHelper {
    return {
        ns,
        get moneyAvailable() {
            return this.ns.getServerMoneyAvailable("home");
        }
    }
}

async function boostAmount(ns: NS, tgtAmount = 16, print = true) {
    const helper = getHelper(ns);
    const limit = MAXAMOUNT(ns);

    tgtAmount = Math.min(limit, tgtAmount);
    while (ns.hacknet.numNodes() < tgtAmount) {
        const cost = ns.hacknet.getPurchaseNodeCost();
        for (; ;) {
            const moneyAvailable = helper.moneyAvailable;
            if (moneyAvailable >= cost) {
                const res = ns.hacknet.purchaseNode();
                if (res != -1) {
                    if (print) ns.print(`Purchased hacknet Node with index ${res}`);
                    break;
                }
            }
            printWaitingMoney(ns, moneyAvailable, cost, `purchase hacknet node`);
            await ns.sleep(MONEY_WAIT_SLEEP_MS);
        }
        await ns.sleep(1000);
    }

    if (print) ns.tprintf("All %d nodes purchased", tgtAmount);
    return;
}


async function boostLevel(ns: NS, tgtLevel = 80, print = true) {
    const helper = getHelper(ns);
    const n = ns.hacknet.numNodes();

        tgtLevel = Math.min(tgtLevel, MAX_LEVEL);
    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).level < tgtLevel) {
            const cost = ns.hacknet.getLevelUpgradeCost(i, 1);
            for (; ;) {
                const moneyAvailable = helper.moneyAvailable;
                if (moneyAvailable >= cost && ns.hacknet.upgradeLevel(i, 1)) break;

                if (print) printWaitingMoney(ns, moneyAvailable, cost, `upgrade level of hacknet node ${i}`);
                await ns.sleep(MONEY_WAIT_SLEEP_MS);
            }
        }
    }

        ns.toast(`All nodes upgraded to level ${tgtLevel}`, "success");
}

export async function boostRam(ns: NS, tgtRam = 16, print = true) {
    const helper = getHelper(ns);
    const n = ns.hacknet.numNodes();
    tgtRam = Math.min(tgtRam, 64);

    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).ram < tgtRam) {
            const cost = ns.hacknet.getRamUpgradeCost(i, 1);
            for (; ;) {
                const moneyAvailable = helper.moneyAvailable;
                if (moneyAvailable >= cost && ns.hacknet.upgradeRam(i, 1)) break;
                if (print) printWaitingMoney(ns, moneyAvailable, cost, `upgrade RAM of hacknet node ${i}`);
                await ns.sleep(MONEY_WAIT_SLEEP_MS);
            }
        }
    }

        ns.toast(`All nodes upgraded to ${ns.formatRam(tgtRam)} of RAM`, "success");
}

async function boostCores(ns: NS, tgtCores = 4, print = true) {
    const helper = getHelper(ns);
    const n = ns.hacknet.numNodes();
    tgtCores = Math.min(tgtCores, 16);

    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).cores < tgtCores) {
            const cost = ns.hacknet.getCoreUpgradeCost(i, 1);
            for (; ;) {
                const moneyAvailable = helper.moneyAvailable;
                if (moneyAvailable >= cost && ns.hacknet.upgradeCore(i, 1)) break;
                if (print) printWaitingMoney(ns, moneyAvailable, cost, `upgrade cores of hacknet node ${i}`);
                await ns.sleep(MONEY_WAIT_SLEEP_MS);
            }
        }
    }

    ns.toast(`All nodes upgraded to ${tgtCores} cores`, "success");
}

async function maximize(ns: NS, amount: number) {
    await boostAmount(ns, amount);
    await boostLevel(ns, MAX_LEVEL);
    await boostRam(ns, MAX_RAM);
    await boostCores(ns, MAX_CORES);
    return;
}

export async function optimize(ns: NS) {
    const augMults = ns.getHacknetMultipliers();
    const { coreCost, levelCost, production, purchaseCost, ramCost } = augMults;
    const BreakEvenCycles = 20_000;

    function optimizeCores(level: number, ram: number, base = 1) {
        const baseIncome = ns.formulas.hacknetNodes.moneyGainRate(level, ram, base, production);
        for (let i = 1; i + base <= MAX_CORES; i++) {
            const costForNext = ns.formulas.hacknetNodes.coreUpgradeCost(base, i, coreCost);
            const boostIncome = ns.formulas.hacknetNodes.moneyGainRate(level, ram, base + i, production);
            if ((boostIncome - baseIncome) * BreakEvenCycles < costForNext) {
                if (i == 1) {
                    ns.tprint(`INFO: Base core amount ${base} should not be upgraded (based on forecast over ${ns.formatNumber(BreakEvenCycles, 0)} cycles)`);
                }
                return base + i - 1;
            }
        }
        return MAX_CORES;
    }

    function optimizeRam(level: number, cores: number, base = 1) {
        const baseIncome = ns.formulas.hacknetNodes.moneyGainRate(level, base, cores, production);
        let i = 1;
        let n = base * 2;

        for (; n <= MAX_RAM; n *= 2) {
            const costForNext = ns.formulas.hacknetNodes.ramUpgradeCost(base, i, ramCost);
            const boostIncome = ns.formulas.hacknetNodes.moneyGainRate(level, n, cores, production);
            if ((boostIncome - baseIncome) * BreakEvenCycles < costForNext) {
                if (i == 1) {
                    ns.tprint(`INFO: Base ram amount ${base} should not be upgraded (based on forecast over ${ns.formatNumber(BreakEvenCycles, 0)} cycles)`);
                }
                return n / 2;
            }
            i++
        }
        return MAX_RAM;
    }

    function optimizeLevel(ram: number, cores: number, base = 1) {
        const baseIncome = ns.formulas.hacknetNodes.moneyGainRate(base, ram, cores, production);

        for (let i = 1; base + i <= MAX_LEVEL; ++i) {
            const costForNext = ns.formulas.hacknetNodes.levelUpgradeCost(base, i, levelCost);
            const boostIncome = ns.formulas.hacknetNodes.moneyGainRate(base + i, ram, cores, production);
            if ((boostIncome - baseIncome) * BreakEvenCycles < costForNext) {
                if (i == 1) {
                    ns.tprint(`INFO: Base level ${base} should not be upgraded (based on forecast over ${ns.formatNumber(BreakEvenCycles, 0)} cycles)`);
                }
                return base + i - 1;
            }
        }
        return MAX_LEVEL;
    }

    let level = 1;
    let ram = 1;
    let cores = 1;

    for (let i = 0; i < 5; i++) {
        const oldLevel = level;
        level = optimizeLevel(ram, cores, level);
        if (oldLevel !== level) {
            const oldRam = ram;
            ram = optimizeRam(level, cores, ram);
            if (oldRam !== ram) {
                const oldCores = cores;
                cores = optimizeCores(level, ram, cores);
                if (oldCores != cores) {
                    continue;
                }
            }
        }
        break;
    }

    ns.print(`Settled on lvl ${level}, ${ns.formatRam(ram)} RAM, ${cores} cores...`)

    const prod = ns.formulas.hacknetNodes.moneyGainRate(level, ram, cores, production);
    const forLevel = ns.formulas.hacknetNodes.levelUpgradeCost(1, level - 1, levelCost);
    const forRam = ns.formulas.hacknetNodes.ramUpgradeCost(1, Math.log2(ram), ramCost);
    const forCore = ns.formulas.hacknetNodes.coreUpgradeCost(1, cores - 1, coreCost);
    const perNode = forLevel + forRam + forCore;
    const expectedYield = (prod * BreakEvenCycles) - perNode;

    ns.print(`INFO: Expected yield of ${expectedYield} optimal after ${BreakEvenCycles} of production with lvl ${level}, ${ns.formatRam(ram)} RAM, ${cores} cores.`);
    ns.print(`WARN: Current optimization path will spend $${ns.formatNumber(perNode)} per node, on top of purchase cost`);

    const maxNodes = ns.hacknet.maxNumNodes();
    const startNodes = ns.hacknet.numNodes();
    for (let i = 1; startNodes + i <= maxNodes; i++) {
        const nextCost = ns.hacknet.getPurchaseNodeCost();
        if (nextCost > expectedYield) {
            ns.print(`Stopping after purchasing ${i - 1} extra nodes...`);
            break;
        }
        await boostAmount(ns, startNodes + i);
        await boostLevel(ns, level);
        await boostRam(ns, ram);
        await boostCores(ns, cores);

        await ns.sleep(100);
    }

        await boostRam(ns, ram);
        await boostLevel(ns, level);
        await boostCores(ns, cores);

        if (ns.hacknet.numNodes() > 0) {
            ns.toast(`After optimization, have ${ns.hacknet.numNodes()} hacknet nodes.`, 'success', 5000);
        }
        else {
            ns.toast(`No further hacknet nodes purchased, as it would not be profitable based on ${BreakEvenCycles} cycles of forecast earnings`, 'warning', 5000);
        }
    return;
}

export async function main(ns: NS) {
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("sleep");

    switch (ns.args[0]) {
        case "max":
            await maximize(ns, Number(ns.args[1]) ?? ns.hacknet.numNodes());
            return;
        case "amount":
            await boostAmount(ns, Number(ns.args[1]) ?? Math.max(8, ns.hacknet.numNodes() / 2 + 4));
            return;
        case "level":
            await boostLevel(ns, Number(ns.args[1]) ?? 80);
            return;
        case "ram":
            await boostRam(ns, Number(ns.args[1]) ?? 8);
            return;
        case "cores":
            await boostCores(ns, Number(ns.args[1]) ?? 4);
            return;
        case "forecast":
            forecastEarnings(ns);
            return;
        case "init":
        case "optimize":
        default:
            await boostAmount(ns, 8);
            await optimize(ns);
            return;
    }
}

function forecastEarnings(ns: NS) {
    const sources = ns.getMoneySources().sinceInstall;
    const totalSpent = sources.hacknet_expenses;

    let prodRate = 0;
    let totalProd = 0;

    const numNodes = ns.hacknet.numNodes();
    for (let i = 0; i < numNodes; i++) {
        const stats = ns.hacknet.getNodeStats(i);
        prodRate += stats.production;
        totalProd += stats.totalProduction;
    }

    const balance = totalProd + totalSpent;
    if (balance < 0) {
        const curtime = ns.getTimeSinceLastAug() / 1000;
        const delta = Math.abs(balance) / prodRate;
        const beventime = curtime + delta;
        ns.tprint(`WARN: HackNet currently in deficit by ${ns.formatNumber(balance)}`);
        ns.tprint(`WARN: Current Aug-Epoch Time:\t ${formatTime(curtime)}`);
        ns.tprint(`WARN: Break-Even Aug-Epoch Time:\t ${formatTime(beventime)} (after ${formatTime(delta)})`);
    }
    ns.tprint(`INFO: Balance will be ${ns.formatNumber(projectAfter(balance, prodRate, 60 * 60))} in 1h`);
    ns.tprint(`INFO: Balance will be ${ns.formatNumber(projectAfter(balance, prodRate, 6 * 60 * 60))} in 6h`);
    ns.tprint(`INFO: Balance will be ${ns.formatNumber(projectAfter(balance, prodRate, 12 * 60 * 60))} in 12h`);
    ns.tprint(`INFO: Balance will be ${ns.formatNumber(projectAfter(balance, prodRate, 24 * 60 * 60))} in 24h`);

    ns.tprint(`INFO: Balance will be +$1m in ${formatTime(projectUntilBalance(balance, prodRate, 1_000_000))}`);
    ns.tprint(`INFO: Balance will be +$10m in ${formatTime(projectUntilBalance(balance, prodRate, 10_000_000))}`);
    ns.tprint(`INFO: Balance will be +$100m in ${formatTime(projectUntilBalance(balance, prodRate, 100_000_000))}`);
    ns.tprint(`INFO: Balance will be +$1b in ${formatTime(projectUntilBalance(balance, prodRate, 1_000_000_000))}`);
}

function projectAfter(startingBalance: number, growRate: number, elapsed: number): number {
    return startingBalance + (growRate * elapsed);
}

function projectUntilBalance(startingBalance: number, growRate: number, targetBalance: number): number {
    if (startingBalance >= targetBalance) return 0;

    return Math.ceil((targetBalance - startingBalance) / growRate);
}
