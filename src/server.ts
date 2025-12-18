import { AutocompleteData, NS } from "@ns";

const cmds = [
    "init",
    "report",
    "once",
    "loop",
    "balance",
];

const getName = (i: number) => `pserv-${i}`;

interface RamStats {
    _: { [key: number]: string[] };
    minRam: number;
    maxRam: number;
}

function getRamStats(ns: NS): RamStats {
    const ret: RamStats = { _: {}, minRam: Infinity, maxRam: 0 };
    const servers = getServersByName(ns);
    for (const serv of servers) {
        const ram = ns.getServerMaxRam(serv);
        if (ram < ret.minRam) {
            ret.minRam = ram;
        }
        if (ram > ret.maxRam) {
            ret.maxRam = ram;
        }
        if (ret._[ram] === undefined) {
            ret._[ram] = [];
        }
        ret._[ram].push(serv);
    }
    return ret;
}

function getServersByName(ns: NS): string[] {
    const limit = ns.getPurchasedServerLimit();
    const servers: string[] = [];
    for (let i = 0; i < limit; i++) {
        const name = getName(i);
        if (ns.serverExists(name)) {
            servers.push(name);
        }
    }
    return servers;
}

function getNumberPurchaseableServers(ns: NS): number {
    const limit = ns.getPurchasedServerLimit();
    let nExtra = 0;
    for (let i = 0; i < limit; i++) {
        const name = getName(i);
        if (!ns.serverExists(name)) {
            nExtra++
        }
    }
    return nExtra;
}

export async function purchaseMaximum(ns: NS, ram = 8): Promise<boolean> {
    let mutated = false;

    // Iterator we'll use for our loop
    let i = 0;

    // Continuously try to purchase servers until we've reached the maximum
    // amount of servers
    while (i < ns.getPurchasedServerLimit()) {
        const servname = getName(i);
        if (ns.serverExists(servname)) {
            i++;
            ns.print(`Skipping ${servname} (already exists)...`);
            continue;
        }
        // Check if we have enough money to purchase a server
        if (ns.getServerMoneyAvailable("home") > ns.getPurchasedServerCost(ram)) {
            mutated = true;
            const hostname = ns.purchaseServer(servname, ram);
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


export async function balanceServerRam(ns: NS): Promise<boolean> {
    if (getServersByName(ns).length === 0) {
        ns.toast("No servers to balance", "error");
        return false;
    }
    const { minRam, maxRam } = getRamStats(ns);
    let ret = false;
    if (minRam === maxRam) {
        ns.toast("All servers are balanced already", "info");
        return false;
    } else {
        const servers = getServersByName(ns);
        for (const server of servers) {
            if (await upgradeServerRam(ns, server, maxRam)) {
                ret = true;
            }
        }
    }
    return ret;
}

export async function upgradeServerRam(ns: NS, server: string, targetRam: number): Promise<boolean> {
    const currentRam = ns.getServerMaxRam(server);
    if (currentRam < targetRam) {
        const costToUpgrade = ns.getPurchasedServerUpgradeCost(server, targetRam);
        if (costToUpgrade > 0) {
            const moneyNeeded = costToUpgrade;
            let moneyAvailable: number;
            ns.disableLog("sleep");
            ns.disableLog("getServerMoneyAvailable");
            do {
                moneyAvailable = ns.getServerMoneyAvailable("home");
                if (moneyAvailable < moneyNeeded) {
                    ns.print(`Waiting for money to upgrade ${server}: $${ns.formatNumber(moneyAvailable)}/$${ns.formatNumber(moneyNeeded)} (${ns.formatPercent(moneyAvailable / moneyNeeded)}, Need $${ns.formatNumber(moneyNeeded - moneyAvailable)} more)`);
                    await ns.sleep(1000);
                }
            } while (moneyAvailable < moneyNeeded);
            ns.enableLog("sleep");
            ns.enableLog("getServerMoneyAvailable");
            if (ns.upgradePurchasedServer(server, targetRam)) {
                ns.print(`SUCCESS: ${server} RAM increased from ${ns.formatRam(currentRam)} ===> ${ns.formatRam(targetRam)}`);
                return true;
            } else {
                ns.tprint(`ERROR: could not upgrade ${server} from ${ns.formatRam(currentRam)} ===> ${ns.formatRam(targetRam)}`);
            }
        }
    }
    return false;
}

export async function doubleAllServerRam(ns: NS): Promise<boolean> {
    const servers = getServersByName(ns);
    const ramLimit = ns.getPurchasedServerMaxRam();
    let numUpgrades = 0;
    for (const serv of servers) {
        const currentRam = ns.getServerMaxRam(serv);
        const targetRam = currentRam * 2;
        if (targetRam > ramLimit) {
            ns.printf("server %s already at max ram (%s)", serv, ns.formatRam(ramLimit));
            continue;
        } else {
            if (await upgradeServerRam(ns, serv, targetRam)) {
                numUpgrades++;
            } else {
                ns.tprintf("script error: unable to purchase %s upgrade for server %s", ns.formatRam(targetRam), serv);
                continue;
            }
        }
    }
    return (numUpgrades > 0);
}

function serverReport(ns: NS) {
    ns.tprint("Server RAM Report:");
    const servers = getServersByName(ns);
    const extra = getNumberPurchaseableServers(ns);
    const stats = getRamStats(ns);
    ns.tprintf("Total servers: %d", servers.length);
    if (extra > 0) {
        ns.tprintf("Number of remaining servers to purchase: %d", extra);
    }
    let ramDex = stats.maxRam;
    if (ramDex === 0) {
        ns.tprint(`INFO: no servers have been purchased`);
        return;
    }
    while (ramDex >= stats.minRam) {
        if ((stats._[ramDex] !== undefined) && stats._[ramDex].length > 0) {
            ns.tprintf("Servers with %s RAM: %d", ns.formatRam(ramDex), stats._[ramDex].length);
        }
        ramDex /= 2;
    }
    return;
}

export async function main(ns: NS): Promise<void> {
    // ns.disableLog("ALL");
    async function once() {
        await doubleAllServerRam(ns);
    }
    async function init() {
        if (await purchaseMaximum(ns, 8)) {
            ns.toast("Bought all available 8GB servers", "success");
        }
    }
    switch (ns.args[0] ?? "once") {
        case "report":
            serverReport(ns);
            return;
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
            const flags = ns.flags([["n", 0]]);
            if (Number.isSafeInteger(flags.n)) {
                const nTimes = Number(flags.n);
                if (nTimes > 0) {
                    for (let i = 0; i < nTimes; i++) {
                        ns.clearLog();
                        await once();
                    }
                    return;
                } else {
                    for (; ;) {
                        ns.clearLog();
                        await once();
                    }
                }
            } else {
                ns.tprint("ERROR: -n requires a non-negative integer");
                return;
            }
        default:
            await init();
            await once();
            break;
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    if (args.length > 0) {
        if (args[0] == "loop") {
            data.flags([["n", 0]]);
        }
        return [];
    } else {
        return [...cmds];
    }
}
