import { NS, PlayerRequirement, Skills } from '@ns';
import { red, reset } from './helper';

/**
 * Shows the raw requirements for getting a faction invitation
 * TODO - add progress indication for individual requirements
 * @param ns
 */
export async function main(ns: NS) {
    ns.disableLog('ALL');
    ns.clearLog();
    ns.tail();
    const tgt = ns.args[0] as string ?? "Bladeburners";
    const reqs = ns.singularity.getFactionInviteRequirements(tgt);
    ns.print(`=== ${tgt} Invite Requirements ===`);
    showReqs(ns, reqs);
}

function showReqs(ns: NS, reqs: PlayerRequirement[]) {
    for (const req of reqs) {
        showReq(ns, req);
    }
}

function showReq(ns: NS, req: PlayerRequirement, level = 0, negated = false) {
    function levelPrint(msg: string) {
        if (!negated) {
            ns.print(`${'\t'.repeat(level)}  * ${msg}`);
        } else {
            ns.print(`${'\t'.repeat(level)}  * ${red}${msg}${reset}`);
        }
    }
    switch (req.type) {
        case 'backdoorInstalled':
            levelPrint(`Backdoor installed on ${req.server}`);
            break;
        case 'bitNodeN':
            levelPrint(`On BitNode ${req.bitNodeN}`);
            break;
        case 'bladeburnerRank':
            levelPrint(`Bladeburner rank ${req.bladeburnerRank}`);
            break;
        case 'city':
            levelPrint(`In ${req.city}`);
            break;
        case 'companyReputation':
            levelPrint(`Reputation ${req.reputation} with ${req.company}`);
            break;
        case 'employedBy':
            levelPrint(`Employed by ${req.company}`);
            break;
        case 'everyCondition':
            levelPrint(`All of the following:`);
            req.conditions.forEach(cond => showReq(ns, cond, level + 1, negated));
            break;
        case 'file':
            levelPrint(`Have file ${req.file}`);
            break;
        case 'hacknetCores':
            levelPrint(`Total HackNet cores >= ${req.hacknetCores}`);
            break;
        case 'hacknetLevels':
            levelPrint(`Total HackNet level >= ${req.hacknetLevels}`);
            break;
        case 'hacknetRAM':
            levelPrint(`Total HackNet RAM >= ${ns.formatRam(req.hacknetRAM)}`);
            break;
        case 'jobTitle':
            levelPrint(`${req.jobTitle} of any company`);
            break;
        case 'karma':
            levelPrint(`${req.karma} Karma`);
            break;
        case 'location':
            levelPrint(`In ${req.location}`);
            break;
        case 'money':
            levelPrint(`Have $${ns.formatNumber(req.money)}`);
            break;
        case 'not':
            showReq(ns, req.condition, level, !negated);
            break;
        case 'numAugmentations':
            levelPrint(`Have ${req.numAugmentations} augmentations`);
            break;
        case 'numInfiltrations':
            levelPrint(`Performed ${req.numInfiltrations} infiltrations`);
            break;
        case 'numPeopleKilled':
            levelPrint(`Killed ${req.numPeopleKilled} people`);
            break;
        case 'skills':
            showSkills(ns, req.skills, level)
            break;
        case 'someCondition':
            levelPrint(`Any of the following:`);
            req.conditions.forEach(cond => showReq(ns, cond, level + 1, negated));
            break;
        case 'sourceFile':
            levelPrint(`Have SourceFile ${req.sourceFile}`);
            break;
    }
}

function showSkills(ns: NS, skills: Partial<Skills>, level: number) {
    function levelPrint(msg: string) {
        ns.print(`${'\t'.repeat(level)}  * ${msg}`);
    }
    for (const [skill, lvl] of Object.entries(skills)) {
        if (typeof lvl === 'number') {
            levelPrint(`${skill} skill level ${lvl}`);
        }
    }
}
