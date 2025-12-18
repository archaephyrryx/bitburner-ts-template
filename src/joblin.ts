import { NS, CompanyName, AutocompleteData } from '@ns';
import { Who } from './jobber';
import { MegacorpNames } from './global';

export async function main(ns: NS) {
    await workAt(ns, ns.args[0] as CompanyName, ns.args[1] as Who);
    return;
}

function hasLessRep(ns: NS, corp1: CompanyName, corp2: CompanyName): boolean {
    const corp1Rep = ns.singularity.getCompanyRep(corp1);
    const corp2Rep = ns.singularity.getCompanyRep(corp2);

    return (corp1Rep < corp2Rep);
}


function factionNeedsRep(ns: NS, factionName: string): boolean {
    const factionRep = ns.singularity.getFactionRep(factionName);
    const ownedAugs = ns.singularity.getOwnedAugmentations(true);
    const factionAugs = ns.singularity.getAugmentationsFromFaction(factionName);
    let maxReqRep = 0;
    for (const augName of factionAugs) {
        if (ownedAugs.includes(augName)) {
            continue;
        }
        const reqRep = ns.singularity.getAugmentationRepReq(augName);
        if (reqRep > maxReqRep) {
            maxReqRep = reqRep;
        }
    }
    return (factionRep < maxReqRep);
}


async function workAt(ns: NS, where: CompanyName, who: Who): Promise<boolean> {
    if (typeof who === 'number') {
        const nSleeves = ns.sleeve.getNumSleeves();
        if (who >= nSleeves) {
            ns.tprint("ERROR: cannot ask sleeve " + who + " to work, as it does not exist...");
        } else {
            while (!ns.sleeve.setToCompanyWork(who, where)) {
                await ns.sleep(1000);
            }
            return true;
        }
    } else if (who === "self") {
        if (ns.bladeburner.inBladeburner() && ns.bladeburner.getCurrentAction()?.type !== "Idle") return false;
        const task = ns.singularity.getCurrentWork();
        if (task !== null) {
            switch (task.type) {
                case "GRAFTING":
                case "CREATE_PROGRAM":
                    return false;
                case "FACTION":
                    if (factionNeedsRep(ns, task.factionName)) {
                        return false;
                    }
                    break;
                case "COMPANY":
                    if (hasLessRep(ns, task.companyName, where)) {
                        return false;
                    }
                    break;
                case "CRIME":
                    break;
            }
            const timeOut = 5;
            ns.ui.openTail();
            for (let n = timeOut; n > 0; n--) {
                ns.toast(`Will cancel current work in ${n} seconds. Kill this process (${ns.getScriptName()} ${ns.args.join(' ')}) that opens to protect important work.`, `warning`, 1000);
                await ns.sleep(1000);
            }
        }
        ns.singularity.workForCompany(where);
    }
    return true;
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    if (args.length === 1 && MegacorpNames.includes(args[0] as typeof MegacorpNames[number])) {
        return [0, 1, 2, 3, 4, 5, 6, 7, "self"];
    } else {
        return [...MegacorpNames] as string[];
    }
}
