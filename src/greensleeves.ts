import { NS, SleevePerson, SleeveTask } from "@ns";

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
            ns.tprint(`${_crime}: ${crimeInfo.workers} sleeves with ${crimeInfo.timesCompleted} attempts at ${ns.formatNumber(crimeInfo.timesCompleted / crimeInfo.sleeveCycles, 2)} tasks/cycle`)
        }
    }
}

function checkInfo(crimeStats: CrimeInfo | undefined): crimeStats is CrimeInfo {
    if (typeof crimeStats === 'undefined') {
        return false;
    }
    return true;
}
