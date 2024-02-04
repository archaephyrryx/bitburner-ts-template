import { NS, CompanyName } from "@ns";


export function allocateWorker(ns: NS, company: CompanyName): [number, boolean] {
    const nSleeves = ns.sleeve.getNumSleeves();
    let candidate = -1;
    for (let i = 0; i < nSleeves; i++) {
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
