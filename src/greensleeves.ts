import { NS, SleevePerson, SleeveTask, CompanyName, AutocompleteData, FactionWorkType } from "@ns";
import { uniqSort } from "./util/arraytools";
import { getFactionRepProgress, FactionRepProgress } from "./factoid";
import { CompanyInfo, getCompanyInfo, MegacorpNames } from './global';

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
        case "faction":
            await initiateFaction(ns, nSleeves);
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

function shouldWorkCompany(companyName: CompanyName, infos: CompanyInfo[]) {
    const tmp = findInfo(infos, companyName);
    const maxRep = Math.max(...infos.map((ci) => ci.rep));
    if (tmp === undefined) {
        return true;
    }
    return (!tmp.hasFaction || (tmp.rep == maxRep && maxRep > 0));
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

function findInfo(infos: CompanyInfo[], name: `${CompanyName}`): CompanyInfo | undefined {
    return infos.find((ci) => ci.corp === name);
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return ["work", "crime", "faction"];
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
        ns.tprint(`WARNING: No free sleeves to engage in company work...`);
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
            ns.tprint(`WARNING: Out of useful factions to assign to sleeve ${sleeveIx}, falling back on crime...`);
            ns.sleeve.setToCommitCrime(sleeveIx, "Shoplift");
        }
    }
}

type FactionMicrocosm = { factionName: string, deltaRep: number[] };

function getFactionsByPriority(ns: NS, table: FactionRepProgress[], limit = -1) {
    const unsorted: FactionMicrocosm[] = [];
    for (const entry of table) {
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
    const micros = unsorted.filter((x) => x.deltaRep.length > 0).toSorted((a, b) => a.deltaRep[0] - b.deltaRep[0]);
    if (limit >= 0 && micros.length > limit) {
        return micros.slice(0, limit);
    } else {
        return micros;
    }
}
function setSleeveToFactionWork(ns: NS, sleeveIx: number, factionName: string): boolean {
    const types = ["field", "hacking", "security"];
    for (const workType of types) {
        if (ns.sleeve.setToFactionWork(sleeveIx, factionName, workType as FactionWorkType)) {
            return true;
        }
    }
    return false;
}
