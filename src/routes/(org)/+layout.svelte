<script lang="ts">
    import { flip } from "svelte/animate";
    import OrgThemeChooser, { current_theme } from "./orgThemeChooser.svelte";
    // import type { LayoutServerData } from "./$types";
    import "app-css";
    import resolveConfig from "tailwindcss/resolveConfig";
    import tailwindConfig from "tailwind-config";
    const { theme } = resolveConfig(tailwindConfig);
    // NOTE: https://stackoverflow.com/questions/70983798/how-to-use-tailwindcsss-resolve-config-with-typescript
    // NOTE: https://github.com/tailwindlabs/tailwindcss/issues/9929
    // NOTE: https://github.com/tailwindlabs/tailwindcss/pull/9972#issuecomment-1644475526
    // NOTE: seems still not fixed
    import { screens } from "tailwindcss/defaultTheme";
    const lg = +Object.entries((theme as any)?.screens ?? {}) // HACK
        .reduce<string>((acc, [k, v]) => {
            if (k == "lg") {
                if (typeof v == "string") return v;
                else throw "unexpected: lg is not string";
            } else return acc;
        }, screens.lg)
        .slice(0, -2);
    // export let data: LayoutServerData;
    let innerWidth: number;
    $: reach_lg = innerWidth > lg;
</script>

<svelte:window bind:innerWidth />

<div
    class="bg-background dark:bg-dark-background {$current_theme}
           grid gap-1"
    style="grid: auto auto /1fr 4fr 1fr;"
>
    {#each [1] as _ (1)}
        <div
            animate:flip
            class="px-5 py-3
                   row-start-2 row-span-1 col-span-full
                   lg:row-span-full lg:col-start-2 lg:col-span-1
                   lg:rounded-2xl"
        >
            <slot />
        </div>
    {/each}
    {#each [0] as _ (reach_lg)}
        <aside
            class="px-5 py-3
                   row-start-1 row-span-1 col-span-full
                   lg:col-start-3"
        >
            <OrgThemeChooser slideDirection={reach_lg ? "y" : "x"} />
        </aside>
    {/each}
</div>

<style lang="postcss">
    .solarized {
        --background-highlight: theme(colors.solarized.base2);
        --background: theme(colors.solarized.base3);
        --content-emphasized: theme(colors.solarized.base01);
        --content: theme(colors.solarized.base00);
        --content-secondary: theme(colors.solarized.base1);
        --dark-background-highlight: theme(colors.solarized.base02);
        --dark-background: theme(colors.solarized.base03);
        --dark-content-emphasized: theme(colors.solarized.base1);
        ---dark-content: theme(colors.solarized.base0);
        --dark-content-secondary: theme(colors.solarized.base01);
        --section-title: theme(colors.solarized.green);
    }
    .dark-academia {
        --background-highlight: theme(
            colors.dark-academia.charcoal-gray.DEFAULT
        );
        --background: theme(colors.dark-academia.grayish-blue.DEFAULT);
        --content-emphasized: theme(colors.solarized.base01);
        --content: theme(colors.dark-academia.charcoal-gray.DEFAULT);
        --content-secondary: theme(colors.solarized.base1);
        --dark-background-highlight: theme(
            colors.dark-academia.grayish-blue.DEFAULT
        );
        --dark-background: theme(colors.dark-academia.charcoal-gray.DEFAULT);
        --dark-content-emphasized: theme(colors.solarized.base1);
        ---dark-content: theme(colors.dark-academia.grayish-blue.DEFAULT);
        --dark-content-secondary: theme(colors.solarized.base01);
    }
</style>
