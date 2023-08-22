<script lang="ts">
    import { flip } from "svelte/animate";
    import OrgThemeChooser, {
        readable_current_theme,
        readable_min_width,
    } from "./orgThemeChooser.svelte";
    import type { LayoutServerData } from "./$types";

    export let data: LayoutServerData;

    let screen_width: number;

    $: theme_chooser_on_the_right_side = screen_width / 6 > $readable_min_width;
</script>

<svelte:window bind:innerWidth={screen_width} />

{@html data[$readable_current_theme] ?? ""}
<div style="display: grid; grid: auto auto /1fr 4fr 1fr; gap: 10px 10px">
    {#each [1] as _ (1)}
        <div
            animate:flip
            style:grid-area={theme_chooser_on_the_right_side
                ? "1/2/3/3"
                : "2/1/3/4"}
        >
            <slot />
        </div>
    {/each}
    {#each [0] as _ (theme_chooser_on_the_right_side)}
        <div
            style:grid-area={theme_chooser_on_the_right_side
                ? "1/3/2/4"
                : "1/1/2/4"}
        >
            <OrgThemeChooser
                flexDirection={theme_chooser_on_the_right_side
                    ? "column"
                    : "row"}
            />
        </div>
    {/each}
</div>

<style>
</style>
