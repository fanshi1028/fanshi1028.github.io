<script lang="ts">
    import { flip } from "svelte/animate";
    import OrgThemeChooser, {
        readable_current_theme,
    } from "./orgThemeChooser.svelte";
    import type { LayoutServerData } from "./$types";
    import "../../app.css";
    import resolveConfig from "tailwindcss/resolveConfig";
    import tailwindConfig from "../../../tailwind.config";
    const { theme } = resolveConfig(tailwindConfig);
    // NOTE: https://stackoverflow.com/questions/70983798/how-to-use-tailwindcsss-resolve-config-with-typescript
    // NOTE: https://github.com/tailwindlabs/tailwindcss/issues/9929
    // NOTE: https://github.com/tailwindlabs/tailwindcss/pull/9972#issuecomment-1644475526
    // NOTE: seems still not fixed
    import { screens } from "tailwindcss/defaultTheme";
    const lg = +Object.entries(theme?.screens ?? {})
        .reduce<string>((acc, [k, v]) => {
            if (k == "lg") {
                if (typeof v == "string") return v;
                else throw "unexpected: lg is not string";
            } else return acc;
        }, screens.lg)
        .slice(0, -2);
    export let data: LayoutServerData;
    let innerWidth: number;
    $: reach_lg = innerWidth > lg;
</script>

<svelte:window bind:innerWidth />

{@html data[$readable_current_theme] ?? ""}
<div style="display:grid; grid: auto auto /1fr 4fr 1fr; gap: 1px">
    {#each [1] as _ (1)}
        <div
            animate:flip
            class="row-start-2 row-span-1 col-span-full \
                   lg:row-span-full lg:col-start-2 lg:col-span-1"
        >
            <slot />
        </div>
    {/each}
    {#each [0] as _ (reach_lg)}
        <div
            class="row-start-1 row-span-1 col-span-full \
                   lg:col-start-3"
        >
            <OrgThemeChooser slideDirection={reach_lg ? "y" : "x"} />
        </div>
    {/each}
</div>

<style>
</style>
