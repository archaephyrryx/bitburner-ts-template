import { CompanyName, Task, NS, SleeveBladeburnerTask, SleeveClassTask, SleeveCompanyTask, SleeveCrimeTask, SleeveFactionTask, SleeveInfiltrateTask, SleeveRecoveryTask, SleeveSupportTask, SleeveSynchroTask } from '@ns';
import { CityName, FactionWorkType, GymLocationName, GymType, LocationName } from './global.enums';
export * from './global.enums';

export type NodeInfo = { name: string; skill: number; ports: number };

export type GymStat = GymType | `${GymType}`;

export interface TaskInfo {
    task: Task | null;
    isIdle: boolean;
    isFocused: boolean;
    isGrafting: boolean;
    //  REVIEW -  there might be more simple heuristics we care about than just gym-stats
    gymStatsIncreased: GymStat[];
    locationLocked: boolean;
}

export function getTaskInfo(ns: NS, task?: Task | null): TaskInfo {
    if (task === undefined) task = ns.singularity.getCurrentWork();
    if (task === null) {
        return {
            task: null,
            isIdle: true,
            isFocused: false,
            isGrafting: false,
            gymStatsIncreased: [],
            locationLocked: false,
        };
    }
    const isFocused = ns.singularity.isFocused();
    const isIdle = false;
    switch (task.type) {
        case "GRAFTING":
            return {
                task: task,
                isFocused,
                isIdle,
                isGrafting: true,
                gymStatsIncreased: [],
                locationLocked: false,
            };
        case "CLASS":
            if (task.location in Object.values(GymLocationName)) {
                const gymStatsIncreased: GymStat[] = [];
                const gymStat = task.classType.substring(0, 3).toLowerCase();
                if (!(gymStat in Object.values(GymType))) {
                    ns.alert(`ERROR: unrecognized gym stat ${task.classType} in getTaskInfo`);
                } else {
                    gymStatsIncreased.push(gymStat as GymStat);
                };
                return {
                    task: task,
                    isIdle,
                    isFocused,
                    isGrafting: false,
                    gymStatsIncreased: gymStatsIncreased,
                    locationLocked: true,
                };
            } else {
                return {
                    task: task,
                    isIdle,
                    isFocused,
                    isGrafting: false,
                    gymStatsIncreased: [],
                    locationLocked: true,
                };
            }
        case "COMPANY":
            return {
                task: task,
                isIdle,
                isFocused,
                isGrafting: false,
                gymStatsIncreased: [],
                locationLocked: false,
            };
        case "CRIME":
            // WIP - some crimes increase stats
            return {
                task: task,
                isIdle,
                isFocused,
                isGrafting: false,
                gymStatsIncreased: [],
                locationLocked: false,
            };
        case "FACTION":
            const gymStatsIncreased: GymStat[] = [];
            switch (task.factionWorkType) {
                case FactionWorkType.hacking:
                    break;
                case FactionWorkType.security:
                case FactionWorkType.field:
                    gymStatsIncreased.push(...Object.values(GymType));
                    break;
            }
            return {
                task: task,
                isIdle,
                isFocused,
                isGrafting: false,
                gymStatsIncreased: gymStatsIncreased,
                locationLocked: false,
            };
        case 'CREATE_PROGRAM':
            return {
                task: task,
                isIdle,
                isFocused,
                isGrafting: false,
                gymStatsIncreased: [],
                locationLocked: false,
            };
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

export enum MegaCorp {
    ECorp = CompanyName.ECorp,
    MegaCorp = CompanyName.MegaCorp,
    BachmanAndAssociates = CompanyName.BachmanAndAssociates,
    BladeIndustries = CompanyName.BladeIndustries,
    NWO = CompanyName.NWO,
    ClarkeIncorporated = CompanyName.ClarkeIncorporated,
    OmniTekIncorporated = CompanyName.OmniTekIncorporated,
    FourSigma = CompanyName.FourSigma,
    KuaiGongInternational = CompanyName.KuaiGongInternational,
    FulcrumTechnologies = CompanyName.FulcrumTechnologies,
}

export const MegacorpNames: `${CompanyName}`[] = [...Object.values(MegaCorp)];

// TODO - refactor to make it harder to get unexpected  error-throws
export function corpFaction(corp: string): string {
    if (corp === "Fulcrum Technologies") {
        return "Fulcrum Secret Technologies";
    } else if (MegacorpNames.includes(corp as `${CompanyName}`)) {
        return corp;
    }
    throw new Error(`Corporation ${corp} not recognized or has no faction.`);
}

export const Cities: `${CityName}`[] = [...Object.values(CityName)];

/**
 * Maps a university location to its corresponding city.
 * @param university The university location to map.
 * @returns The city name the university is located in, or undefined if not found.
 */
export function universityToCity(university: LocationName | `${LocationName}`): CityName | undefined {
    switch (university) {
        case LocationName.AevumSummitUniversity:
            return CityName.Aevum;
        case LocationName.Sector12RothmanUniversity:
            return CityName.Sector12;
        case LocationName.VolhavenZBInstituteOfTechnology:
            return CityName.Volhaven;
        default:
            return undefined;
    }
}
