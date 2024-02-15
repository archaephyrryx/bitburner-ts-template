import { NS } from "@ns";
import { getWork } from "./global";

export const MinAugments = 5;

export async function main(ns: NS) {
    const force = ns.args[0] === "force";
    const installed = ns.singularity.getOwnedAugmentations(false);
    const all = ns.singularity.getOwnedAugmentations(true);
    const queuedAugs = all.filter((a) => !installed.includes(a));
    if (queuedAugs.length === 0) {
        ns.tprint(`ERROR: ${ns.getScriptName()} is intended to install augments, but none are queued!`);
        return;
    }
    if (queuedAugs.length < MinAugments && !force) {
        ns.tprint(`WARNING: By default, ${ns.getScriptName()} does nothing if fewer than ${MinAugments} augmentations are queued (only ${queuedAugs.length} currently)...`);
        ns.tprint(`INFO: To circumvent this guard, run as: ${ns.getScriptName()} force`);
        return;
    }
    // begin sunsetting
    ns.scriptKill("market.js", "home");
    ns.exec("market.js", "home", {}, "autosell", "--force");
    await ns.sleep(5000);
    while (ns.singularity.upgradeHomeCores()) {
        await ns.sleep(100);
    }
    while (ns.singularity.upgradeHomeRam()) {
        await ns.sleep(100);
    }
    ns.exec("augs.js", "home", {}, "buy-avail", "--neuro");
    if (ns.singularity.exportGameBonus()) {
        ns.singularity.exportGame();
    }

    ns.toast("WILL SUNSET IN 1 MINUTE", "warning", 60_000);
    await ns.sleep(60_000);
    for (; ;) {
        const work = getWork(ns);
        if (work !== null && work.type === "GRAFTING") {
            ns.tail();
            ns.clearLog();
            ns.print("WARNING: Currently grafting, will not sunset until grafting is aborted or finishes...");
            await ns.sleep(1000);
            continue;
        } else {
            break;
        }
    }

    ns.singularity.installAugmentations("bootstrap.js");
}
