import { NS, CompanyName, FactionWorkType } from "@ns";
import { getFactionRepProgress, FactionRepProgress } from "./factoid";
import { CompanyInfo, getCompanyInfo, MegacorpNames, universityToCity } from './global';
import { fallbackAction, fallbackCrime, moveToCity } from "./sleeve-man.consts";

const SYNCHRO_THRESHOLD = 50;
const SHOCK_THRESHOLD = 90;

export async function main(ns: NS) {
    if (ns.isRunning("sleeve-man.blade.js") && ns.args[0] !== "--force") ns.exit();
    const nSleeves = ns.sleeve.getNumSleeves();
    await initiateFaction(ns, nSleeves);
}

function shouldWorkCompany(companyName: CompanyName, infos: CompanyInfo[]) {
    const tmp = findInfo(infos, companyName);
    const maxRep = Math.max(...infos.map((ci) => ci.rep));
    if (tmp === undefined) {
        return true;
    }
    return (!tmp.hasFaction || (tmp.rep == maxRep && maxRep > 0));
}

function findInfo(infos: CompanyInfo[], name: `${CompanyName}`): CompanyInfo | undefined {
    return infos.find((ci) => ci.corp === name);
}

async function initiateFaction(ns: NS, count: number) {
    const canAssign: number[] = [];
    const infos = getCompanyInfo(ns, MegacorpNames);
    scan: for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task !== null) {
            cond: switch (task.type) {
                case 'COMPANY':
                    if (shouldWorkCompany(task.companyName, infos)) {
                        continue scan;
                    } else {
                        canAssign.push(i);
                    }
                    break cond;
                case 'BLADEBURNER':
                case 'INFILTRATE':
                    continue scan;
                case 'FACTION':
                    if (ns.sleeve.getSleeve(i).shock > 0) {
                        ns.sleeve.setToShockRecovery(i);
                    } else {
                        ns.sleeve.setToIdle(i);
                    }
                    canAssign.push(i);
                    break cond;
                case 'CLASS':
                case 'CRIME':
                    canAssign.push(i);
                    break cond;
                case 'SYNCHRO':
                    if (ns.sleeve.getSleeve(i).sync >= SYNCHRO_THRESHOLD) {
                        canAssign.push(i)
                    }
                    break cond;
                case 'RECOVERY':
                    if (ns.sleeve.getSleeve(i).shock <= SHOCK_THRESHOLD) {
                        canAssign.push(i)
                    }
                    break cond;
                case 'SUPPORT':
                    break cond;
            }
        } else {
            canAssign.push(i);
        }
    }

    if (canAssign.length === 0) {
        ns.print(`WARNING: No free sleeves to engage in company work...`);
        return;
    }

    const prog = getFactionRepProgress(ns);
    const priorities = getFactionsByPriority(ns, prog, canAssign.length);

    let prioIndex = 0;

    for (const sleeveIx of canAssign) {
        if (prioIndex < priorities.length) {
            const { factionName, deltaRep } = priorities[prioIndex];
            if (setSleeveToFactionWork(ns, sleeveIx, factionName)) {
                ns.print(`SUCCESS: Sleeve ${sleeveIx} now working for ${factionName} (rep until next aug: ${ns.formatNumber(deltaRep[0])})`);
                prioIndex++;
            } else {
                ns.print(`WARNING: Unable to set sleeve ${sleeveIx} to work unspecified work-type at faction ${factionName}, skipping...`)
                prioIndex++;
            }
        } else {
            if (ns.sleeve.getSleeve(sleeveIx).shock > 0) {
                ns.sleeve.setToShockRecovery(sleeveIx);
            } else {
                switch (fallbackAction.type) {
                    case "CLASS":
                        {
                            if (fallbackAction.location !== undefined) {
                                const dest = universityToCity(fallbackAction.location);
                                if (dest === undefined) {
                                    ns.tprint(`ERROR: ${fallbackAction.location} is not a known university!`);
                                } else if (!moveToCity(ns, sleeveIx, dest)) {
                                    ns.tprint(`ERROR: Unable to get sleeve ${sleeveIx} to travel to dest!`);
                                } else if (!ns.sleeve.setToUniversityCourse(sleeveIx, fallbackAction.location, fallbackAction.classType ?? "Computer Science")) {
                                    ns.tprint(`ERROR: Unable to set sleeve ${sleeveIx} to study ${fallbackAction.classType} at ${fallbackAction.location}...`)
                                } else {
                                    ns.print(`SUCCESS: Set sleeve ${sleeveIx} to study ${fallbackAction.classType} at ${fallbackAction.location}!`)
                                    break;
                                }
                                ns.sleeve.setToCommitCrime(sleeveIx, fallbackCrime);
                                break;
                            } else {
                                continue;
                            }
                        }
                    case "CRIME":
                        // eslint-disable-next-line no-case-declarations
                        const currentTask = ns.sleeve.getTask(sleeveIx);
                        if (currentTask != null && currentTask.type == "CRIME" && currentTask.crimeType == fallbackAction.crimeType) {
                            continue;
                        }
                        if (!ns.sleeve.setToCommitCrime(sleeveIx, fallbackAction.crimeType ?? "Shoplift")) {
                            if (!ns.sleeve.setToCommitCrime(sleeveIx, fallbackCrime)) {
                                ns.tprint(`ERROR: Unable to set sleeve ${sleeveIx} to commit either specified or fallback crime`);
                            } else {
                                ns.tprint(`WARNING: Falling back from specified crime ${fallbackAction.crimeType} to ${fallbackCrime}...`);
                            }
                        }
                        break;
                    default:
                        ns.tprint(`WARNING: unhandled logic for fallback action ${fallbackAction}`);
                        break;
                }
            }
        }
    }
}

type FactionMicrocosm = { factionName: string, deltaRep: number[] };

function getFactionsByPriority(ns: NS, table: FactionRepProgress[], limit = -1) {
    const unsorted: FactionMicrocosm[] = [];
    for (const entry of table) {
        if (ns.gang.inGang() && ns.gang.getGangInformation().faction === entry.factionName) {
            continue;
        }
        const { factionName, augReqs, myRep } = entry;
        const deltaRep: number[] = [];
        if (augReqs.length > 0) {
            const sortedAugs = augReqs.toSorted((a, b) => a.reqRep - b.reqRep);
            for (const aug of sortedAugs) {
                const delta = aug.reqRep - myRep;
                if (delta > 0)
                    deltaRep.push(delta);
            }
        }
        unsorted.push({ factionName, deltaRep });
    }
    const micros = unsorted.filter((x) => x.deltaRep.length > 0 && x.factionName !== "Shadows of Anarchy").toSorted((a, b) => a.deltaRep[0] - b.deltaRep[0]);
    if (limit >= 0 && micros.length > limit) {
        return micros.slice(0, limit);
    } else {
        return micros;
    }
}
function setSleeveToFactionWork(ns: NS, sleeveIx: number, factionName: string): boolean {
    const types = ["field", "security", "hacking"];
    for (const workType of types) {
        if (ns.sleeve.setToFactionWork(sleeveIx, factionName, workType as FactionWorkType)) {
            return true;
        }
    }
    return false;
}
