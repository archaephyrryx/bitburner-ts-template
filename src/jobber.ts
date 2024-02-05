import { NS, CompanyName, Player } from "@ns";
import { allocateWorker } from "./worker";
import { mimicPad } from "./util/stringtools";

export type Who = "self" | number;
export let Working: { [k in CompanyName]?: boolean };

const FactionUnlockRep = 400_000;


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

const WidestName = MegacorpNames.toSorted((a, b) => b.length - a.length)[0];

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

    const reassignCooldownMinutes = 60;

    for (let i = 0; ; i++) {
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

        if (i % reassignCooldownMinutes === 0) {
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
            }
        }


        // wait 1 minute before cycling to avoid clobbering previous reassignments; will reassign every reassignCooldownMinutes cycles
        ns.clearLog();
        for (const corp of MegacorpNames) {
            const favor = ns.singularity.getCompanyFavor(corp);
            const favorGain = ns.singularity.getCompanyFavorGain(corp);
            const rep = ns.singularity.getCompanyRep(corp);
            const hasFaction = ns.getPlayer().factions.includes(corpFaction(corp));
            if (hasFaction) {
                ns.print(`INFO: ${mimicPad(WidestName, corp)} at ${ns.formatNumber(rep)} rep\t${ns.formatNumber(favor, 0)} (+${ns.formatNumber(favorGain + favor - Math.floor(favor), 0)}) favor (Faction: UNLOCKED)`)
            } else {
                ns.print(`WARN: ${mimicPad(WidestName, corp)} at ${ns.formatNumber(rep)} rep\t${ns.formatNumber(favor, 0)} (+${ns.formatNumber(favorGain + favor - Math.floor(favor), 0)}) favor (Faction: ${ns.formatPercent(rep / FactionUnlockRep)})`)
            }
        }
        await ns.sleep(1000 * 60);
    }
}
