import { AutocompleteData, NS, ScriptArg } from "@ns";

function helpText(ns: NS): never {
    ns.tprint("This script helps you find a server on the network and shows you the path to get to it.");
    ns.tprint(`Usage: run ${ns.getScriptName()} [SERVER]`);
    ns.tprint("Example:");
    ns.tprint(`> run ${ns.getScriptName()} n00dles`);
    return ns.exit();
}

function selectTarget(ns: NS, flags: { [key: string]: ScriptArg | string[]; }): string {
    if (Array.isArray(flags._) && flags._.length > 0 && ns.serverExists(flags._[0])) {
        return flags._[0];
    }
    if (ns.fileExists('dispatch-pid.txt')) {
        const pid = Number(ns.read('dispatch-pid.txt') || "0");
        if (pid == 0) {
            ns.tprint("dispatch-pid.txt is empty");
            ns.exit();
        }
        if (ns.getRunningScript(pid)?.pid == pid) {
            const target = ns.read('target.txt');
            if (ns.serverExists(target)) {
                return target;
            } else {
                ns.tprint(`target.txt: "${target}" is not a real server`);
                ns.exit();
            }
        }
    }
    return helpText(ns);

}

export async function main(ns: NS) {
    const flags = ns.flags([
        ['refreshrate', 200],
        ['help', false],
    ])
    if (flags.help) {
        return helpText(ns);
    }
    ns.tail();
    ns.disableLog('ALL');
    const server = selectTarget(ns, flags);
    // eslint-disable-next-line no-constant-condition
    while (true) {
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