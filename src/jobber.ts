import { NS, CompanyName, Player } from "@ns";
import { allocateWorker } from "./worker";

export type Who = "self" | number;
export let Working: { [k in CompanyName]?: boolean };

const MegacorpNames: `${CompanyName}`[] = [
    "ECorp",
    "MegaCorp",
    "Bachman & Associates",
    "Blade Industries",
    "NWO",
    "Clarke Incorporated",
    "OmniTek Incorporated",
    "Four Sigma",
    "KuaiGong International",
    "Fulcrum Technologies",
];

function corpFaction(corp: string): string {
    if (corp === "Fulcrum Technologies") {
        return "Fulcrum Secret Technologies";
    } else if (MegacorpNames.includes(corp as `${CompanyName}`)) {
        return corp;
    }
    throw new Error(`Corporation ${corp} not recognized or has no faction.`);
}

export async function main(ns: NS) {
    ns.tail();
    ns.disableLog('singularity.applyToCompany');
    Working = {};
    const workers: Who[] = [];

    const plyr: Player = ns.getPlayer();

    for (const corp of MegacorpNames) {
        const success = ns.singularity.applyToCompany(corp, "Software");
        if (success) {
            const [who, done] = allocateWorker(ns, corp as CompanyName);
            if (done) {
                Working[corp as CompanyName] = true;
                workers.push(who);
            } else if (who >= 0) {
                ns.exec(ns.getScriptName(), "home", {}, "work", corp, who);
                continue;
            }
        } else if (typeof ns.getPlayer().jobs[corp as CompanyName] !== "undefined") {
            continue;
        } else {
            ns.tprint(`WARNING: unable to automatically enroll for job at ${corp}`);
        }
    }

    for (; ;) {
        const missingWorkers = [];

        for (const corp of MegacorpNames) {
            if (ns.singularity.applyToCompany(corp, "Software")) {
                ns.toast(`Accepted promotion at ${corp}!`, "success", 2500);
            }
            if (Working[corp as CompanyName] ?? false) {
                Working[corp] = false;
                continue;
            } else {
                missingWorkers.push(corp);
            }
        }

        for (const corp of missingWorkers) {
            if (plyr.factions.includes(corpFaction(corp))) {
                continue;
            }
            const [who, done] = allocateWorker(ns, corp as CompanyName);
            if (done) {
                Working[corp as CompanyName] = true;
                workers.push(who);
            } else if (who > 0) {
                if (!ns.exec("joblin.js", "home", {}, corp, who)) {
                    ns.toast("not enough ram to exec joblin...", "warning", 3000);
                }
                continue;
            } else {
                if (!ns.exec("joblin.js", "home", {}, corp, "self")) {
                    ns.toast("not enough ram to exec joblin...", "warning", 3000);
                }
            }
            // wait 1 minute before cycling to avoid clobbering previous reassignments
            await ns.sleep(1000 * 60);
        }

        // wait one hour between batches of reassignments
        await ns.sleep(60 * 60 * 1000);
    }
}
