import { NS, CompanyName, Player } from "@ns";
import { mimicPad } from "./util/stringtools";
import { CompanyInfo, getCompanyInfo, MegacorpNames } from "./global";
import { H, M } from './helper';

const bold = "\u001b[01m";
const reset = "\u001b[0m";

export type Who = "self" | number;

const FactionUnlockRep = 400_000;
const SecondsBetweenResleeve = M;
const SecondsBetweenSelfAssign = H;

const WidestName = MegacorpNames.toSorted((a, b) => b.length - a.length)[0];

function getSleeveCompanies(ns: NS): `${CompanyName}`[] {
    const nSleeves = ns.sleeve.getNumSleeves();
    const ret: `${CompanyName}`[] = [];
    for (let i = 0; i < nSleeves; i++) {
        const task = ns.sleeve.getTask(i);
        if (task !== null && task.type == 'COMPANY') {
            ret.push(`${task.companyName}`);
        }
    }
    return ret;
}

export async function main(ns: NS) {
    ns.tail();
    ns.disableLog('singularity.applyToCompany');
    ns.disableLog('exec');
    ns.disableLog('sleep');

    for (let i = 0; ; i++) {
        const plyr: Player = ns.getPlayer();
        for (const corp of MegacorpNames) {
            if (ns.singularity.applyToCompany(corp, "Software")) {
                ns.toast(`Accepted position or promotion at ${corp}!`, "success", 2500);
            }
        }

        if (i % SecondsBetweenResleeve === 0) {
            const gsPid = ns.exec("greensleeves.js", "home", {}, "work");

            if (gsPid === 0) {
                ns.tprint("WARNING: unable to run `greensleeves.js work` on home");
            }
        }

        if (i % SecondsBetweenSelfAssign === 0) {
            const hasWorkers = getSleeveCompanies(ns);

            const missingWorkers: `${CompanyName}`[] = [];
            for (const corp of MegacorpNames) {
                if (plyr.jobs[corp] !== undefined) {
                    if (!hasWorkers.includes(corp)) {
                        missingWorkers.push(corp);
                    }
                }
            }

            if (missingWorkers.length > 0) {
                let candidate: CompanyInfo | null = null;
                let backup: CompanyInfo | null = null;

                const infos = getCompanyInfo(ns, missingWorkers);
                for (const info of infos) {
                    if (info.hasFaction) {
                        if (backup === null || (backup.hasFaction && backup.nextFavor > info.nextFavor)) {
                            backup = info;
                        }
                        continue;
                    }
                    if (candidate === null) {
                        candidate = info;
                    } else {
                        if (candidate.nextFavor > info.nextFavor || (candidate.nextFavor == info.nextFavor && candidate.rep > info.rep)) {
                            candidate = info;
                        } else if (info.rep >= FactionUnlockRep * 0.50 && info.rep > candidate.rep) {
                            candidate = info;
                        } else {
                            continue;
                        }
                    }
                }
                const finalist = (candidate === null) ? ((backup === null) ? missingWorkers[0] : backup.corp) : candidate.corp;
                if (ns.exec("joblin.js", "home", {}, finalist, "self") === 0) {
                    ns.toast("not enough ram to exec joblin...", "warning", 3000);
                }
            }
        }

        ns.clearLog();
        const infos = getCompanyInfo(ns, MegacorpNames);
        const maxRep = infos.toSorted((a, b) => b.rep - a.rep)[0].rep;
        // wait 1 minute before cycling to avoid clobbering previous reassignments; will reassign every AssignmentCooldownMinutes cycles
        for (const info of infos) {
            if (info.hasFaction) {
                ns.print(`INFO: ${mimicPad(WidestName, info.corp)} at ${ns.formatNumber(info.rep)} rep\t${ns.formatNumber(info.favor, 0)} (+${ns.formatNumber(info.nextFavor - info.favor, 0)}) favor (Faction: UNLOCKED)`)
            } else if (plyr.jobs[info.corp as `${CompanyName}`] !== undefined) {
                if (info.rep == maxRep) {
                    ns.print(`${bold}WARN: ${mimicPad(WidestName, info.corp)} at ${ns.formatNumber(info.rep)} rep\t${ns.formatNumber(info.favor, 0)} (+${ns.formatNumber(info.nextFavor - info.favor, 0)}) favor (Faction: ${ns.formatPercent(info.rep / FactionUnlockRep)})${reset}`)
                } else {
                    ns.print(`WARN: ${mimicPad(WidestName, info.corp)} at ${ns.formatNumber(info.rep)} rep\t${ns.formatNumber(info.favor, 0)} (+${ns.formatNumber(info.nextFavor - info.favor, 0)}) favor (Faction: ${ns.formatPercent(info.rep / FactionUnlockRep)})`)
                }
            } else {
                ns.print(`WARN: ${mimicPad(WidestName, info.corp)} at ${ns.formatNumber(info.rep)} rep\t${ns.formatNumber(info.favor, 0)} (+${ns.formatNumber(info.nextFavor - info.favor, 0)}) favor (NOT ENROLLED IN JOB)`)
            }
        }
        await ns.sleep(1000);
    }
}
