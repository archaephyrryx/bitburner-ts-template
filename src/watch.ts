import { NS } from "@ns";

const watchableScripts = [
    "moneyCount.js",
    "balance.js",
]


function canRun(ns: NS, script: string, hostname: string) {
    const ram = ns.getScriptRam(script, hostname);
    const availableRam = ns.getServerMaxRam(hostname) - ns.getServerUsedRam(hostname);
    return (ram <= availableRam);
}

export async function main(ns: NS) {
    ns.tail();
    const script = ns.args[0] ?? "moneyCount.js";
    if (typeof script === "string") {
        if (!watchableScripts.includes(script)) {
            ns.tprint(`Cannot watch ${script}`);
            return;
        } else if (!ns.fileExists(script, "home")) {
            ns.tprint(`Cannot find ${script}`);
            return;
        } else if (!canRun(ns, script, "home")) {
            ns.tprint(`Cannot run ${script}: need ${ns.getScriptRam(script, "home")} RAM`);
            return;
        }
        let repeatTime = 5000;
        if (Number.isSafeInteger(ns.args[1])) {
            repeatTime = Number(ns.args[1]) * 1000;
        } else if (typeof ns.args[1] === "string") {
            ns.tprint(`Invalid repeat time: ${ns.args[1]}`);
            return;
        }
        ns.tprint(`======== Watch: running ${script} every ${ns.tFormat(repeatTime)} ========`);
        // eslint-disable-next-line no-constant-condition
        while (true) {
            ns.exec(script, "home", 1);
            await ns.sleep(repeatTime);
        }
    }
}
