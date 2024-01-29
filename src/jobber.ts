import { NS, CompanyName } from "@ns";
import { allocateWorker } from "./greensleeves";

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


export async function main(ns: NS) {
    ns.tail();
    ns.disableLog('singularity.applyToCompany');
    let nJobs = 0;
    Working = {};
    const workers: Who[] = [];

    for (const corp of MegacorpNames) {
        const success = ns.singularity.applyToCompany(corp, "Software");
        if (success) {
            nJobs += 1;
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
