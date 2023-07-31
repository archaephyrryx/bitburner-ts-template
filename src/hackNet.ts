import { NS } from "@ns";

async function boostAmount(ns: NS, tgtAmount = 16) {
    while (ns.hacknet.numNodes() < tgtAmount) {
        const res = ns.hacknet.purchaseNode();
        if (res != -1) ns.print("Purchased hacknet Node with index " + res);
        await ns.sleep(1000);
    }

    ns.tprintf("All %d nodes purchased", tgtAmount)
    return;
}


async function boostLevel(ns: NS, tgtLevel = 80) {
    function myMoney() {
        return ns.getServerMoneyAvailable("home");
    }
    const n = ns.hacknet.numNodes();

    tgtLevel = Math.min(tgtLevel, 200);
    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).level < tgtLevel) {
            const cost = ns.hacknet.getLevelUpgradeCost(i, 1);
            while (myMoney() < cost) {
                ns.print("Need $" + cost + " . Have $" + myMoney());
                await ns.sleep(3000);
            }
            ns.hacknet.upgradeLevel(i, 1);
        }
    }

    ns.toast("All nodes upgraded to level " + tgtLevel, "success");
}

export async function boostRam(ns: NS, tgtRam = 16) {
    function myMoney() {
        return ns.getServerMoneyAvailable("home");
    }
    const n = ns.hacknet.numNodes();
    tgtRam = Math.min(tgtRam, 64);

    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).ram < tgtRam) {
            const cost = ns.hacknet.getRamUpgradeCost(i, 1);
            while (myMoney() < cost) {
                ns.print("Need $" + cost + " . Have $" + myMoney());
                await ns.sleep(3000);
            }
            ns.hacknet.upgradeRam(i, 1);
        }
    }

    ns.toast("All nodes upgraded to " + ns.formatRam(tgtRam) + " of RAM", "success");
}

async function boostCores(ns: NS, tgtCores = 4) {
    function myMoney() {
        return ns.getServerMoneyAvailable("home");
    }
    const n = ns.hacknet.numNodes();
    tgtCores = Math.min(tgtCores, 16);

    for (let i = 0; i < n; i++) {
        while (ns.hacknet.getNodeStats(i).cores < tgtCores) {
            const cost = ns.hacknet.getCoreUpgradeCost(i, 1);
            while (myMoney() < cost) {
                ns.print("Need $" + cost + " . Have $" + myMoney());
                await ns.sleep(3000);
            }
            ns.hacknet.upgradeCore(i, 1);
        }
    }

    ns.toast(`All nodes upgraded to ${tgtCores} cores`, "success");
}

export async function boostNodes(ns: NS) {
    await boostAmount(ns, 2);
    await boostLevel(ns, 20);
    await boostAmount(ns, 4);
    await boostLevel(ns, 40);
    await boostRam(ns, 2);
    await boostAmount(ns, 8);
    await boostLevel(ns, 80);
    await boostRam(ns, 4);
    await boostCores(ns, 2);
    await boostRam(ns, 8);
    await boostCores(ns, 4);
    await boostRam(ns, 16);
    await boostCores(ns, 8);
}

async function maximize(ns: NS, amount: number) {
    await boostAmount(ns, amount);
    await boostLevel(ns, 200);
    await boostRam(ns, 64);
    await boostCores(ns, 16);
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
        default:
            await boostNodes(ns);
            return;
    }
}