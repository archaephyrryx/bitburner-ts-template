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
