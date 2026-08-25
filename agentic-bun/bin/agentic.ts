#!/usr/bin/env bun
import { main } from "../src/cli.ts";

process.exitCode = await main(process.argv.slice(2));
