import { CompanyName, CrimeType, FactionWorkType, NS, SleeveBladeburnerTask, SleeveClassTask, SleeveCompanyTask, SleeveCrimeTask, SleeveFactionTask, SleeveInfiltrateTask, SleeveRecoveryTask, SleeveSupportTask, SleeveSynchroTask } from '@ns';

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
export enum CityName {
    Aevum = "Aevum",
    Chongqing = "Chongqing",
    Sector12 = "Sector-12",
    NewTokyo = "New Tokyo",
    Ishima = "Ishima",
    Volhaven = "Volhaven"
}
export const Cities: `${CityName}`[] = [...Object.values(CityName)];

export type SleeveTask =
    | SleeveBladeburnerTask
    | SleeveClassTask
    | SleeveCompanyTask
    | SleeveCrimeTask
    | SleeveFactionTask
    | SleeveInfiltrateTask
    | SleeveRecoveryTask
    | SleeveSupportTask
    | SleeveSynchroTask;

export enum LocationName {
    AevumAeroCorp = "AeroCorp",
    AevumBachmanAndAssociates = "Bachman & Associates",
    AevumClarkeIncorporated = "Clarke Incorporated",
    AevumCrushFitnessGym = "Crush Fitness Gym",
    AevumECorp = "ECorp",
    AevumFulcrumTechnologies = "Fulcrum Technologies",
    AevumGalacticCybersystems = "Galactic Cybersystems",
    AevumNetLinkTechnologies = "NetLink Technologies",
    AevumPolice = "Aevum Police Headquarters",
    AevumRhoConstruction = "Rho Construction",
    AevumSnapFitnessGym = "Snap Fitness Gym",
    AevumSummitUniversity = "Summit University",
    AevumWatchdogSecurity = "Watchdog Security",
    AevumCasino = "Iker Molina Casino",

    ChongqingKuaiGongInternational = "KuaiGong International",
    ChongqingSolarisSpaceSystems = "Solaris Space Systems",
    ChongqingChurchOfTheMachineGod = "Church of the Machine God",

    Sector12AlphaEnterprises = "Alpha Enterprises",
    Sector12BladeIndustries = "Blade Industries",
    Sector12CIA = "Central Intelligence Agency",
    Sector12CarmichaelSecurity = "Carmichael Security",
    Sector12CityHall = "Sector-12 City Hall",
    Sector12DeltaOne = "DeltaOne",
    Sector12FoodNStuff = "FoodNStuff",
    Sector12FourSigma = "Four Sigma",
    Sector12IcarusMicrosystems = "Icarus Microsystems",
    Sector12IronGym = "Iron Gym",
    Sector12JoesGuns = "Joe's Guns",
    Sector12MegaCorp = "MegaCorp",
    Sector12NSA = "National Security Agency",
    Sector12PowerhouseGym = "Powerhouse Gym",
    Sector12RothmanUniversity = "Rothman University",
    Sector12UniversalEnergy = "Universal Energy",

    NewTokyoDefComm = "DefComm",
    NewTokyoGlobalPharmaceuticals = "Global Pharmaceuticals",
    NewTokyoNoodleBar = "Noodle Bar",
    NewTokyoVitaLife = "VitaLife",
    NewTokyoArcade = "Arcade",

    IshimaNovaMedical = "Nova Medical",
    IshimaOmegaSoftware = "Omega Software",
    IshimaStormTechnologies = "Storm Technologies",
    IshimaGlitch = "0x6C1",

    VolhavenCompuTek = "CompuTek",
    VolhavenHeliosLabs = "Helios Labs",
    VolhavenLexoCorp = "LexoCorp",
    VolhavenMilleniumFitnessGym = "Millenium Fitness Gym",
    VolhavenNWO = "NWO",
    VolhavenOmniTekIncorporated = "OmniTek Incorporated",
    VolhavenOmniaCybersystems = "Omnia Cybersystems",
    VolhavenSysCoreSecurities = "SysCore Securities",
    VolhavenZBInstituteOfTechnology = "ZB Institute of Technology",

    Hospital = "Hospital",
    Slums = "The Slums",
    TravelAgency = "Travel Agency",
    WorldStockExchange = "World Stock Exchange",

    Void = "The Void",
} export function universityToCity(university: LocationName | `${LocationName}`): CityName | undefined {
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
