import { AutocompleteData, NS, ScriptArg } from "@ns";

export async function main(ns: NS) {
    const flags = ns.flags([
        ['refreshrate', 200],
        ['help', false],
    ])
    if (Array.isArray(flags._) && flags._.length === 0 || flags.help) {
        ns.tprint("This script helps visualize the money and security of a server.");
        ns.tprint(`USAGE: run ${ns.getScriptName()} SERVER_NAME`);
        ns.tprint("Example:");
        ns.tprint(`> run ${ns.getScriptName()} n00dles`)
        return;
    }
    ns.tail();
    ns.disableLog('ALL');
    // eslint-disable-next-line no-constant-condition
    while (true) {
        const server = (flags._ as string[])[0];
        let money = ns.getServerMoneyAvailable(server);
        if (money === 0) money = 1;
        const maxMoney = ns.getServerMaxMoney(server);
        const minSec = ns.getServerMinSecurityLevel(server);
        const sec = ns.getServerSecurityLevel(server);
        ns.clearLog();
        ns.print(`${server}:`);
        ns.print(` $_______: $${ns.formatNumber(money)} / $${ns.formatNumber(maxMoney)} (${ns.formatPercent(money / maxMoney)})`);
        ns.print(` security: +${(sec - minSec).toFixed(2)}`);
        ns.print(` hack____: ${ns.tFormat(ns.getHackTime(server))} (t=${Math.ceil(ns.hackAnalyzeThreads(server, money))})`);
        ns.print(` grow____: ${ns.tFormat(ns.getGrowTime(server))} (t=${Math.ceil(ns.growthAnalyze(server, maxMoney / money))})`);
        ns.print(` weaken__: ${ns.tFormat(ns.getWeakenTime(server))} (t=${Math.ceil((sec - minSec) * 20)})`);
        await ns.sleep(flags.refreshrate as ScriptArg as number);
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return data.servers;
}