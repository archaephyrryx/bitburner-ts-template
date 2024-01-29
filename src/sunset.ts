import { NS } from "@ns";

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
    while (ns.singularity.upgradeHomeCores()) {
        await ns.sleep(100);
    }
    while (ns.singularity.upgradeHomeRam()) {
        await ns.sleep(100);
    }
    if (ns.singularity.exportGameBonus()) {
        ns.singularity.exportGame();
    }
    ns.singularity.installAugmentations("bootstrap.js");
}