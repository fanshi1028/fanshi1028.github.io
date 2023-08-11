<script lang="ts">
    import { slide } from "svelte/transition";
    import { flip } from "svelte/animate";
    import OrgThemeChooser, {
        readable_current_theme,
        readable_min_width,
    } from "./orgThemeChooser.svelte";
    import type { LayoutServerData } from "./$types";

    export let data: LayoutServerData;

    let post_width: number, screen_width: number;
    $: side_bar_width = (screen_width - post_width) / 2;

    $: theme_chooser_on_the_right_side =
        side_bar_width > $readable_min_width * 1.2;
</script>

<svelte:window bind:innerWidth={screen_width} />

{@html data[$readable_current_theme] ?? ""}
{#key theme_chooser_on_the_right_side}
    <div
        class:theme_chooser_on_the_right_side
        style:--right={theme_chooser_on_the_right_side
            ? `${(side_bar_width - $readable_min_width) / 3}px`
            : null}
        transition:slide
    >
        <OrgThemeChooser
            flexDirection={theme_chooser_on_the_right_side ? "column" : "row"}
        />
    </div>
{/key}
{#each [1] as _ (1)}
    <div bind:clientWidth={post_width} animate:flip>
        <slot />
    </div>
{/each}

<style>
    .theme_chooser_on_the_right_side {
        position: fixed;
        right: var(--right);
        top: 2em;
        z-index: 1;
    }
</style>
