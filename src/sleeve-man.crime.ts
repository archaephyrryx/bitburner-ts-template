import { AutocompleteData, NS } from "@ns";

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

export async function main(ns: NS) {
    const crimeType = ns.args[0] ?? "Shoplift";
    const nSleeves = ns.sleeve.getNumSleeves();
    await initiateCrime(ns, nSleeves, crimeType as `${CrimeType}`);
}

async function initiateCrime(ns: NS, count: number, crime: `${CrimeType}` = "Shoplift") {
    const canAssign: number[] = [];

    for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task === null) {
            ns.sleeve.setToCommitCrime(i, crime);
        } else {
            switch (task.type) {
                case 'FACTION':
                case 'BLADEBURNER':
                case 'INFILTRATE':
                case 'COMPANY':
                    break;
                case 'CLASS':
                case 'CRIME':
                    canAssign.push(i);
                    break;
                case 'SYNCHRO':
                    if (ns.sleeve.getSleeve(i).sync >= SYNCHRO_THRESHOLD) {
                        canAssign.push(i)
                    }
                    break;
                case 'RECOVERY':
                    if (ns.sleeve.getSleeve(i).shock <= SHOCK_THRESHOLD) {
                        canAssign.push(i)
                    }
                    break;
                case 'SUPPORT':
                    break;
            }
        }
    }

    for (const sleeveIx of canAssign) {
        const oldTask = ns.sleeve.getTask(sleeveIx);
        if (oldTask != null && oldTask.type == "CRIME" && oldTask.crimeType == crime) {
            continue;
        }
        ns.sleeve.setToCommitCrime(sleeveIx, crime);
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...Object.values(CrimeType).map((value) => `"${value}"`)];
}
