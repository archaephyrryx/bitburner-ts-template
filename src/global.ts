import { CompanyName, CrimeType, FactionWorkType, NS } from '@ns';

export type NodeInfo = { name: string; skill: number; ports: number };

export interface StudyTask {
    type: "CLASS";
    cyclesWorked: number;
    classType: string;
    location: string;
}

export interface CompanyWorkTask {
    type: "COMPANY";
    cyclesWorked: number;
    companyName: CompanyName;
}

export interface CreateProgramWorkTask {
    type: "CREATE_PROGRAM";
    cyclesWorked: number;
    programName: string;
}
export interface GraftingTask {
    type: "GRAFTING";
    cyclesWorked: number;
    augmentation: string;
}

export interface FactionWorkTask {
    type: "FACTION";
    cyclesWorked: number;
    factionWorkType: FactionWorkType;
    factionName: string;
}

export interface CrimeTask {
    type: "CRIME";
    cyclesWorked: number;
    crimeType: CrimeType;
}

export type WorkTask = StudyTask | CompanyWorkTask | CreateProgramWorkTask | CrimeTask | FactionWorkTask | GraftingTask;

export function getWork(ns: NS): WorkTask | null {
    const task = ns.singularity.getCurrentWork();
    if (task === null) return null;
    return task as WorkTask;
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

export const MegacorpNames: `${CompanyName}`[] = [
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

export function corpFaction(corp: string): string {
    if (corp === "Fulcrum Technologies") {
        return "Fulcrum Secret Technologies";
    } else if (MegacorpNames.includes(corp as `${CompanyName}`)) {
        return corp;
    }
    throw new Error(`Corporation ${corp} not recognized or has no faction.`);
}
