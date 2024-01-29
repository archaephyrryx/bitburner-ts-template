import { NS, SleevePerson, SleeveTask, SleeveFactionTask, SleeveCompanyTask, CompanyName } from "@ns";

const factionWork = (factionName: string, factionWorkType = "field") => ({ type: "FACTION", factionWorkType, factionName })

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
    const nSleeves = ns.sleeve.getNumSleeves();
    const sleeveInfos: Array<SleeveInfo> = [];
    for (let i = 0; i < nSleeves; i++) {
        const task = ns.sleeve.getTask(i);
        if (task === null || task === undefined) {
            ns.sleeve.setToCommitCrime(i, "Shoplift");
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

export function allocateWorker(ns: NS, company: CompanyName): [number, boolean] {
    const nSleeves = ns.sleeve.getNumSleeves();
    let candidate = -1;
    for (let i = 0; i < nSleeves; i++) {
        const slv = ns.sleeve.getSleeve(i);
        const task = ns.sleeve.getTask(i);
        if (task === null) {
            candidate = i;
            continue;
        }
        switch (task.type) {
            case 'SYNCHRO':
            case 'RECOVERY':
            case 'INFILTRATE':
                continue;
            case 'FACTION':
                // eslint-disable-next-line no-case-declarations
                const playerAugs = ns.singularity.getOwnedAugmentations(true);
                if (ns.singularity.getAugmentationsFromFaction(task.factionName).every((aug) => playerAugs.includes(aug))) {
                    if (candidate == -1) {
                        candidate = i;
                    }
                }
                break;
            case 'COMPANY':
                if (task.companyName == company) {
                    return [i, true];
                } else if (ns.singularity.getCompanyRep(task.companyName) > ns.singularity.getCompanyRep(company)) {
                    if (candidate == -1) {
                        candidate = i;
                    }
                }
                continue;
            case 'BLADEBURNER':
                continue;
            case 'CLASS':
            case 'CRIME':
                if (candidate == -1) {
                    candidate = i;
                }
                continue;
            case 'SUPPORT':
                continue;
        }
    }
    if (candidate > -1 && ns.sleeve.setToCompanyWork(candidate, company)) {
        return [candidate, true];
    }
    return [candidate, false];
}
