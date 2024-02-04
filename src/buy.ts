import { AutocompleteData, NS } from "@ns";
import { BUDGET, makePurchase } from "./budget";

const cmds = ['tor', 'wse', 'tix-api', '4s', '4s-api'];
// TODO: check costs once on new bitnode
const TORCOST = 200_000;
const WSECOST = 200_000_000;
const DATA4SCOST = 1_000_000_000;
const TIXCOST = 5_000_000_000;
const API4SCOST = 25_000_000_000;

async function buyTor(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.singularity.purchaseTor(); });
    const id = await BUDGET.request(ns, TORCOST);
    return makePurchase(ns, id, f);
}

async function buyWSE(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchaseWseAccount(); });
    const id = await BUDGET.request(ns, WSECOST);
    return makePurchase(ns, id, f);
}


async function buyTIX(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchaseTixApi(); });
    const id = await BUDGET.request(ns, TIXCOST);
    return makePurchase(ns, id, f);
}

async function buy4SD(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchase4SMarketData(); });
    const id = await BUDGET.request(ns, DATA4SCOST);
    return makePurchase(ns, id, f);
}

async function buy4SApi(ns: NS): Promise<boolean> {
    const f = (async (ns: NS) => { return ns.stock.purchase4SMarketDataTixApi(); });
    const id = await BUDGET.request(ns, API4SCOST);
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
    switch (ns.args[0]) {
        case 'tor':
            announce(ns, "TOR Router", await buyTor(ns));
            return;
        case 'wse':
            announce(ns, "WSE Account", await buyWSE(ns));
            return;
        case 'tix':
            announce(ns, "TIX API Access", await buyTIX(ns));
            return;
        case '4s':
            announce(ns, "4S Market Data", await buy4SD(ns));
            return;
        case '4s-api':
            announce(ns, "4S Market Data TIX API", await buy4SApi(ns));
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
