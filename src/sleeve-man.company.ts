import { NS, CompanyName } from "@ns";
import { uniqSort } from "./util/arraytools";
import { getFactionRepProgress, FactionRepProgress } from "./factoid";
import { getCompanyInfo, MegacorpNames } from './global';

const SYNCHRO_THRESHOLD = 50;
const SHOCK_THRESHOLD = 90;

const fallbackCrime = "Homicide";

export async function main(ns: NS) {
    const nSleeves = ns.sleeve.getNumSleeves();
    await initiateWork(ns, nSleeves);
}

async function initiateWork(ns: NS, count: number) {
    const prog = getFactionRepProgress(ns);

    const canAssign: number[] = [];

    scan: for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task !== null) {
            cond: switch (task.type) {
                case 'FACTION':
                    if (shouldWorkFaction(task.factionName, prog)) {
                        continue scan;
                    } else {
                        canAssign.push(i);
                    }
                    break cond;
                case 'BLADEBURNER':
                case 'INFILTRATE':
                    continue scan;
                case 'COMPANY':
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

    const availableCorps = [...MegacorpNames];

    if (canAssign.length === 0) {
        ns.tprint(`WARNING: No free sleeves to engage in company work...`);
        return;
    }

    const infos = getCompanyInfo(ns, availableCorps);

    const factionLess = infos.filter((info) => !info.hasFaction);

    let selective;

    if (factionLess.length > 0) {
        selective = factionLess;
    } else {
        selective = infos;
    }

    const byRep = selective.toSorted((a, b) => a.rep - b.rep);

    const minRep = byRep[0].corp;
    const maxRep = byRep[byRep.length - 1].corp;

    const byFavor = selective.toSorted((a, b) => a.favor - b.favor);

    const minFavor = byFavor[0].corp;
    const maxFavor = byFavor[byRep.length - 1].corp;

    const order = [maxFavor, minFavor, maxRep, minRep];

    let priorities: string[] = [];
    for (let i = 0; i < order.length; i++) {
        if (priorities.length === canAssign.length) {
            break;
        }
        priorities = uniqSort([...priorities, order[i]]);
    }

    const rest = infos.filter((info) => !priorities.includes(info.corp));

    let prioIndex = 0;
    let restIx = 0;

    for (const sleeveIx of canAssign) {
        if (prioIndex < priorities.length) {
            ns.sleeve.setToCompanyWork(sleeveIx, priorities[prioIndex++] as `${CompanyName}`);
        } else if (restIx < rest.length) {
            ns.sleeve.setToCompanyWork(sleeveIx, rest[restIx++].corp as `${CompanyName}`);
        } else {
            if (ns.sleeve.getSleeve(sleeveIx).shock > 0) {
                ns.sleeve.setToShockRecovery(sleeveIx);
            } else {
                ns.sleeve.setToCommitCrime(sleeveIx, fallbackCrime);
            }
        }
    }
}

function shouldWorkFaction(faction: string, progress: FactionRepProgress[]): boolean {
    const tmp = findProgress(progress, faction);
    if (tmp === undefined) {
        return true;
    }
    const { augReqs, myRep } = tmp;
    const sortedAugs = augReqs.toSorted((a, b) => a.reqRep - b.reqRep);
    const needsMore = sortedAugs.filter((augReq) => augReq.reqRep > myRep);

    if (needsMore.length === 0) {
        return false;
    }

    for (const aug of needsMore) {
        switch (aug.otherSources.kind) {
            case 'exclusive':
                return true;
            case 'factions':
                if (aug.otherSources.numberJoined > 0) {
                    for (const otherFaction in aug.otherSources.otherFactions) {
                        const info = findProgress(progress, otherFaction);
                        if (info === undefined) {
                            continue;
                        } else {
                            if (info.myRep >= aug.reqRep) {
                                return false;
                            }
                        }
                    }
                }
                return true;
        }
    }
    return false;
}

function findProgress(progs: FactionRepProgress[], name: string): FactionRepProgress | undefined {
    return progs.find((frp) => frp.factionName === name);
}
