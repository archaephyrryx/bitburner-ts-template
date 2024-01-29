import { NS, CompanyName } from '@ns';
import { Who, Working } from './jobber';

export async function main(ns: NS) {
    workAt(ns, ns.args[0] as CompanyName, ns.args[1] as Who);
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
            Working[where as CompanyName] = true;
            return true;
        }
    } else if (who === "self") {
        const busy = ns.singularity.isBusy();
        const task = ns.singularity.getCurrentWork();
        if (busy) {
            if ((task as { type: string }).type !== "GRAFTING") {
                const timeOut = 5;
                ns.tail();
                for (let n = timeOut; n > 0; n--) {
                    ns.toast(`Will cancel current work in ${n} seconds. Kill this process (${ns.getScriptName()} ${ns.args.join(' ')}) that opens to protect important work.`, `warning`, 1000);
                    await ns.sleep(1000);
                }
            }
        }
        ns.singularity.workForCompany(where as CompanyName);
        Working[where as CompanyName] = true;
    }
    return true;
}
