import { AutocompleteData, NS } from '@ns';
import { H, bold, reset } from './helper';

const MONEY_PER_HASH = 250_000;
const HASH_FOR_MONEY = 4;

const TIMEFRAME = 4 * H;

// type UpgradeKind = "level" | "ram" | "cores" | "cache";
// type Item = [Action, number, Yield];
// type Action = { kind: 'PURCHASE' } | { kind: 'UPGRADE', what: UpgradeKind, which: number } | { kind: 'EXCHANGE', nTimes: number };
// type Yield = { kind: 'production', amount: number } | { kind: 'capacity', amount: number }

export async function main(ns: NS) {
    const flags = ns.flags([["autoSpend", false], ["sellOnly", false], ["upgradeCache", false], ["keepFraction", 0.5]]);
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("sleep");
    ns.tail();


    await crawl(ns, flags.autoSpend as boolean, flags.sellOnly as boolean, flags.upgradeCache as boolean, flags.keepFraction as number);
}

async function crawl(ns: NS, autoSpend = false, sellOnly = false, upgradeCache = false, keepFraction: number) {
    for (; ;) {
        const nServers = ns.hacknet.numNodes();
        let nHashes = ns.hacknet.numHashes();

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const [capacity, prod] = hashCapacityProduction(ns, nServers);

        while (nHashes >= (sellOnly ? HASH_FOR_MONEY : Math.max(HASH_FOR_MONEY, capacity * keepFraction)) && ns.hacknet.spendHashes("Sell for Money")) {
            nHashes = ns.hacknet.numHashes();
        }

        ns.clearLog();
        ns.print(`Hashes: ${ns.formatNumber(nHashes, 3)}/${ns.formatNumber(capacity)} (${capacity > 0 ? ns.formatPercent(nHashes / capacity) : "N/A"} | ${ns.formatNumber(prod)} h/s)`)


        if (autoSpend) {
            let additionalPrice = ns.hacknet.getPurchaseNodeCost();
            while (ns.getServerMoneyAvailable("home") >= additionalPrice) {
                if (ns.hacknet.purchaseNode() == -1) {
                    break;
                } else {
                    additionalPrice = ns.hacknet.getPurchaseNodeCost();
                }
            }
        }

        for (let i = 0; i < nServers; i++) {
            let atMax = false;
            if (autoSpend) {
                const shouldLvl = shouldUpgradeLevel(ns, i);
                const shouldRam = shouldUpgradeRam(ns, i);
                const shouldCore = shouldUpgradeCores(ns, i);
                if (shouldLvl && ns.hacknet.upgradeLevel(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} level.`, "success", 1000);
                } else if (shouldRam && ns.hacknet.upgradeRam(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} RAM.`, "success", 1000);
                } else if (shouldCore && ns.hacknet.upgradeCore(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cores.`, "success", 1000);
                } else if (upgradeCache && ns.hacknet.upgradeCache(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cache.`, "success", 1000);
                } else if (!shouldLvl && !shouldRam && !shouldCore) {
                    atMax = true;
                }
            } else {
                const shouldLvl = shouldUpgradeLevel(ns, i);
                const shouldRam = shouldUpgradeRam(ns, i);
                const shouldCore = shouldUpgradeCores(ns, i);
                if (!shouldLvl && !shouldRam && !shouldCore) {
                    atMax = true;
                }
            }
            const stats = ns.hacknet.getNodeStats(i);
            ns.print(`${atMax ? bold : ""}${stats.name}: ${ns.formatNumber(stats.production, 3)} h/s\t ${stats.level.toString().padStart(3, " ")} | ${((stats.ramUsed ?? 0) == 0) ? `${ns.formatRam(stats.ram)}` : `${ns.formatRam(stats.ram)} (used: ${ns.formatRam(stats.ramUsed ?? 0)})`} | ${stats.cores} ${stats.cache === undefined ? "" : `| ${stats.cache} (${ns.formatNumber(stats.hashCapacity ?? 0)})`}${atMax ? reset : ""}`);
        }
        await ns.sleep(200);
    }
}

export function hashCapacityProduction(ns: NS, numNodes: number): [number, number] {
    let totalCap = 0;
    let totalProd = 0;

    for (let i = 0; i < numNodes; i++) {
        const stats = ns.hacknet.getNodeStats(i);
        totalCap += stats.hashCapacity ?? 0;
        totalProd += stats.production;
    }

    return [totalCap, totalProd];
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["autoSpend", false], ["sellOnly", false], ["upgradeCache", false], ["keepFraction", 0.5]]);
    return [];
}

function shouldUpgradeLevel(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getLevelUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel + 1, stats.ramUsed ?? 0, stats.ram, stats.cores, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}

function shouldUpgradeRam(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getRamUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel, stats.ramUsed ?? 0, stats.ram * 2, stats.cores, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}

function shouldUpgradeCores(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getCoreUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel, stats.ramUsed ?? 0, stats.ram, stats.cores + 1, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}
