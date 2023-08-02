import { NS } from "@ns";
import { idealThreads } from "./helper";

export async function purchaseMaximum(ns: NS, ram = 8): Promise<boolean> {
    let mutated = false;

    // Iterator we'll use for our loop
    let i = ns.getPurchasedServers().length;

    // Continuously try to purchase servers until we've reached the maximum
    // amount of servers
    while (i < ns.getPurchasedServerLimit()) {
        // Check if we have enough money to purchase a server
        if (ns.getServerMoneyAvailable("home") > ns.getPurchasedServerCost(ram)) {
            mutated = true;
            const hostname = ns.purchaseServer("pserv-" + i, ram);
            if (hostname === "") {
                ns.toast("Failed to purchase server", "error");
                return mutated;
            }
            ++i;
        }
        //Make the script wait for a second before looping again.
        //Removing this line will cause an infinite loop and crash the game.
        await ns.sleep(1000);
    }
    return mutated;
}

function getMinMaxRam(ns: NS): [number, number] {
    const servers = ns.getPurchasedServers();
    let minRam = Infinity;
    let maxRam = 0;
    for (let i = 0; i < servers.length; i++) {
        const serv = servers[i];
        const ram = ns.getServerMaxRam(serv);
        if (ram < minRam) {
            minRam = ram;
        }
        if (ram > maxRam) {
            maxRam = ram;
        }
    }
    return [minRam, maxRam];
}

export async function balanceServerRam(ns: NS): Promise<boolean> {
    const [minRam, maxRam] = getMinMaxRam(ns);
    if (minRam === maxRam) {
        return false;
    } else {
        for (const server of ns.getPurchasedServers()) {
            if (ns.getServerMaxRam(server) < maxRam) {
                const costToUpgrade = ns.getPurchasedServerUpgradeCost(server, maxRam);
                if (costToUpgrade > 0) {
                    while (ns.getServerMoneyAvailable("home") < costToUpgrade) {
                        await ns.sleep(1000);
                    }
                    if (ns.upgradePurchasedServer(server, maxRam)) {
                        ns.tprintf("Upgraded server %s to %dGB", server, maxRam);
                        continue;
                    } else {
                        ns.tprintf("script error: unable to purchase %dGB upgrade for server %s", maxRam, server);
                        return false;
                    }
                }

            }
        }
    }
    return true;
}

export async function doubleAllServerRam(ns: NS): Promise<boolean> {
    const servers = ns.getPurchasedServers();
    const ramLimit = ns.getPurchasedServerMaxRam();
    let numUpgrades = 0;
    for (const serv of servers) {
        let upgradeCost = 0;
        let targetRam = 8;
        for (; targetRam <= ramLimit; targetRam *= 2) {
            upgradeCost = ns.getPurchasedServerUpgradeCost(serv, targetRam);
            if (upgradeCost > 0) {
                ns.tprintf("Target RAM for server %s: %dGB", serv, targetRam);
                break;
            }
        }
        while (ns.getServerMoneyAvailable("home") < upgradeCost) {
            await ns.sleep(1000);
        }
        if (ns.upgradePurchasedServer(serv, targetRam)) {
            numUpgrades++;
        } else {
            ns.tprintf("script error: unable to purchase %s upgrade for server %s", ns.formatRam(targetRam), serv);
            continue;
        }
    }
    return (numUpgrades > 0);
}

export async function main(ns: NS): Promise<void> {
    async function once() {
        await doubleAllServerRam(ns);
    }
    async function init() {
        if (await purchaseMaximum(ns, 8)) {
            ns.toast("Bought all available 8GB servers", "success");
        }
    }
    switch (ns.args[0] ?? "once") {
        case "init":
            await init();
            break;
        case "once":
            await once();
            break;
        case "balance":
            await balanceServerRam(ns);
            break;
        case "loop":
            if (Number.isSafeInteger(ns.args[1])) {
                const nTimes = Number(ns.args[1]);
                for (let i = 0; i < nTimes; i++) {
                    await once();
                }
                return;
            } else {
                for (; ;) {
                    await once();
                }
            }
        default:
            await init();
            await once();
            break;
    }
}