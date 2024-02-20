import { CodingContract, NS } from "@ns";

export type Solver = (ns: NS, cc: CodingContract, file: string, host: string | undefined) => boolean;
