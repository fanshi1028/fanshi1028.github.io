<script lang="ts">
    import type { LayoutServerData } from "./$types";

    export let data: LayoutServerData;

    import OrgThemeChooser from "./orgThemeChooser.svelte";
    import { org_css, type OrgTheme } from "./orgThemes";

    let current_theme: OrgTheme = "solarized-dark";

    let post_width: number, screen_width: number;
    $: side_bar_width = (screen_width - post_width) / 2;

    let theme_chooser_min_width: number;
    $: theme_chooser_on_the_right_side =
        side_bar_width > theme_chooser_min_width * 1.2;
</script>

<svelte:window bind:innerWidth={screen_width} />
<svelte:head>
    {#if current_theme != "default-minimal"}
        <link rel="stylesheet" type="text/css" href={org_css[current_theme]} />
    {:else}
        {@html data.defaultCSS}
    {/if}
</svelte:head>

<div
    class:theme_chooser_on_the_right_side
    style:--right={theme_chooser_on_the_right_side
        ? `${(side_bar_width - theme_chooser_min_width) / 3}px`
        : null}
>
    <OrgThemeChooser
        bind:current_theme
        flexDirection={theme_chooser_on_the_right_side ? "column" : "row"}
        on:theme_chooser_init|once={({ detail: { min_width } }) =>
            (theme_chooser_min_width = min_width)}
    />
</div>
<div bind:clientWidth={post_width}><slot /></div>

<style>
    .theme_chooser_on_the_right_side {
        position: fixed;
        right: var(--right);
        top: 2em;
        z-index: 1;
    }
</style>
