import { AutocompleteData, NS } from "@ns";
import { BUDGET, makePurchase } from "./budget";

const cmds = ['tor', 'wse', 'tix-api', '4s', '4s-api', 'home-ram', 'home-core'] as const;
// TODO: check costs once on new bitnode
const TORCOST = 200_000;
const WSECOST = 200_000_000;
const DATA4SCOST = 1_000_000_000;
const TIXCOST = 5_000_000_000;
const API4SCOST = 25_000_000_000;

async function buyTor(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.singularity.purchaseTor(); });
    const id = BUDGET.request(ns, TORCOST, "purchase TOR router");
    return makePurchase(ns, id, f);
}

async function buyWSE(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchaseWseAccount(); });
    const id = BUDGET.request(ns, WSECOST, "purchase WSE Account");
    return makePurchase(ns, id, f);
}

async function buyHomeRam(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.singularity.upgradeHomeRam(); });
    const id = BUDGET.request(ns, ns.singularity.getUpgradeHomeRamCost(), "upgrade home RAM");
    return makePurchase(ns, id, f);
}

async function buyHomeCore(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.singularity.upgradeHomeCores(); });
    const id = BUDGET.request(ns, ns.singularity.getUpgradeHomeCoresCost(), "upgrade home cores");
    return makePurchase(ns, id, f);
}


async function buyTIX(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchaseTixApi(); });
    const id = BUDGET.request(ns, TIXCOST, "purchase TIX API");
    return makePurchase(ns, id, f);
}

async function buy4SD(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchase4SMarketData(); });
    const id = BUDGET.request(ns, DATA4SCOST * ns.getBitNodeMultipliers().FourSigmaMarketDataCost, "purchase 4S data");
    return makePurchase(ns, id, f);
}

async function buy4SApi(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchase4SMarketDataTixApi(); });
    const id = BUDGET.request(ns, API4SCOST * ns.getBitNodeMultipliers().FourSigmaMarketDataApiCost, "purchase 4S API");
    return makePurchase(ns, id, f);
}

function announce(ns: NS, what: string, success: boolean) {
    if (success) {
        ns.toast(`Bought (or already owned) ${what}!`, 'success', 3000);
    } else {
        ns.toast(`Unable to purchase ${what}...`, 'error', 3000);
    }
}

export async function main(ns: NS) {
    switch (ns.args[0] as typeof cmds[number]) {
        case 'tor':
            announce(ns, "TOR Router", await buyTor(ns));
            return;
        case 'wse':
            announce(ns, "WSE Account", await buyWSE(ns));
            return;
        case 'tix-api':
            announce(ns, "TIX API Access", await buyTIX(ns));
            return;
        case '4s':
            announce(ns, "4S Market Data", await buy4SD(ns));
            return;
        case '4s-api':
            announce(ns, "4S Market Data TIX API", await buy4SApi(ns));
            return;
        case 'home-ram':
            announce(ns, `Home Ram Upgrade (==>${ns.formatRam(ns.getServerMaxRam("home") * 2)})`, await buyHomeRam(ns));
            return;
        case 'home-core':
            announce(ns, `Home Core Upgrade (==>${ns.getServer("home").cpuCores + 1})`, await buyHomeCore(ns));
            return;
        default:
            ns.tprint(`WARN Usage: ${ns.getScriptName()} [${cmds.join(' | ')}]`);
            return;

    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, ...args: string[]) {
    return [...cmds];
}
