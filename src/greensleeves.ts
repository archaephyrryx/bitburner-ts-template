import { NS, SleevePerson, SleeveTask, CompanyName, AutocompleteData } from "@ns";
import { MegacorpNames, corpFaction } from "./jobber";
import { uniqSort } from "./util/arraytools";
import { getFactionRepProgress, FactionRepProgress } from "./factoid";

const factionWork = (factionName: string, factionWorkType = "field") => ({ type: "FACTION", factionWorkType, factionName })

const SYNCHRO_THRESHOLD = 50;
const SHOCK_THRESHOLD = 90;


enum CrimeType {
    shoplift = "Shoplift",
    robStore = "Rob Store",
    mug = "Mug",
    larceny = "Larceny",
    dealDrugs = "Deal Drugs",
    bondForgery = "Bond Forgery",
    traffickArms = "Traffick Arms",
    homicide = "Homicide",
    grandTheftAuto = "Grand Theft Auto",
    kidnap = "Kidnap",
    assassination = "Assassination",
    heist = "Heist",
}

type SleeveInfo = SleevePerson & { task?: SleeveTask };
type CrimeInfo = { timesCompleted: number, sleeveCycles: number, workers: number };
type CrimeStats = {
    [k in `${CrimeType}`]: CrimeInfo;
};

export async function main(ns: NS) {
    const mode = ns.args[0] ?? "crime";
    const nSleeves = ns.sleeve.getNumSleeves();

    switch (mode) {
        case "work":
            await initiateWork(ns, nSleeves);
            break;
        case "crime":
        default:
            await initiateCrime(ns, nSleeves);
            break;
    }
}

async function initiateCrime(ns: NS, count: number, crime: `${CrimeType}` = "Shoplift") {
    const sleeveInfos: Array<SleeveInfo> = [];
    for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task === null || task === undefined) {
            ns.sleeve.setToCommitCrime(i, crime);
        }
        sleeveInfos.push({ ...ns.sleeve.getSleeve(i), task: ns.sleeve.getTask(i) ?? undefined });
    }

    const crimeStats: Partial<CrimeStats> | CrimeStats = {};

    for (const slv of sleeveInfos) {
        if (slv.task ?? false) {
            switch (slv.task?.type) {
                case "CRIME": {
                    const crimeType = slv.task?.crimeType;
                    let index: `${CrimeType}`;
                    if (typeof crimeType !== "string") {
                        index = CrimeType[crimeType];
                    } else {
                        index = crimeType;
                    }
                    crimeStats[index] = {
                        sleeveCycles: (crimeStats[index]?.sleeveCycles ?? 0) + slv.task.cyclesWorked,
                        timesCompleted: (crimeStats[index]?.timesCompleted ?? 0) + slv.task.tasksCompleted,
                        workers: (crimeStats[index]?.workers ?? 0) + 1
                    };
                    break;
                }
                default:
                    continue;
            }
        }
    }

    for (const _crime in crimeStats) {
        const crime: `${CrimeType}` = _crime as `${CrimeType}`;
        const crimeInfo = crimeStats[crime];
        if (typeof crimeInfo !== 'undefined') {
            if (crimeInfo.sleeveCycles == 0) {
                ns.tprint(`${_crime}: ${crimeInfo.workers} sleeves with ${crimeInfo.timesCompleted} attempts`)
            } else {
                ns.tprint(`${_crime}: ${crimeInfo.workers} sleeves with ${crimeInfo.timesCompleted} attempts at ${ns.formatNumber(crimeInfo.timesCompleted / crimeInfo.sleeveCycles, 2)} tasks/cycle`)
            }
        }
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return ["work", "crime"];
}

async function initiateWork(ns: NS, count: number) {
    const prog = getFactionRepProgress(ns);

    const canAssign: number[] = [];

    scan: for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task !== null) {
            cond: switch (task.type) {
                case 'FACTION':
                    if (shouldWork(task.factionName, prog)) {
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

    const byRep = infos.toSorted((a, b) => a.rep - b.rep);

    const minRep = byRep[0].corp;
    const maxRep = byRep[infos.length - 1].corp;

    const byFavor = infos.toSorted((a, b) => a.favor - b.favor);

    const minFavor = byFavor[0].corp;
    const maxFavor = byFavor[infos.length - 1].corp;

    const order = [minRep, maxRep, minFavor, maxFavor];

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
            ns.tprint(`WARNING: Out of companies to assign to sleeve ${sleeveIx}, falling back on crime...`);
            ns.sleeve.setToCommitCrime(sleeveIx, "Shoplift");
        }
    }
}

export type CompanyInfo = { corp: string, rep: number, favor: number, nextFavor: number, hasFaction: boolean };


export function getCompanyInfo(ns: NS, names: `${CompanyName}`[]): CompanyInfo[] {
    const infos = [];

    for (const corp of names) {
        const favor = ns.singularity.getCompanyFavor(corp);
        const favorGain = ns.singularity.getCompanyFavorGain(corp);

        const nextFavor = favor + favorGain;

        const rep = ns.singularity.getCompanyRep(corp);

        const allFactions = [...ns.getPlayer().factions, ...ns.singularity.checkFactionInvitations()];
        const hasFaction = allFactions.includes(corpFaction(corp));

        infos.push({ corp, rep, favor, nextFavor, hasFaction });
    }

    return infos;
}

function shouldWork(faction: string, progress: FactionRepProgress[]): boolean {
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
