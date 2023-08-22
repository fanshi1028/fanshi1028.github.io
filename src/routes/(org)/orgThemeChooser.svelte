<script lang="ts" context="module">
    const min_width = writable<number>(NaN);
    const current_theme = writable<OrgTheme>("solarizedDark");

    export const readable_min_width = {subscribe: min_width.subscribe};
    export const readable_current_theme = {subscribe: current_theme.subscribe};
</script>

<script lang="ts">
    import { writable } from "svelte/store";
    import { org_css, type OrgTheme } from "./orgThemes";
    import { slide } from "svelte/transition";

    export let flexDirection: "row" | "column";
</script>

<h4 bind:clientWidth={$min_width} style="width: fit-content; padding: 0 0.5em;">
    Choose your eye candy!
</h4>

<div class="theme_chooser" style:--flexDirection={flexDirection}>
    {#each [...Object.keys(org_css), "defaultMinimal"] as theme, idx (idx)}
        <div
            style="padding-left: 1em; padding-bottom: 0.5em; display: flex"
            in:slide|global={{
                duration: 700,
                delay: idx * 200,
                axis: flexDirection == "row" ? "x" : "y",
            }}
        >
            <input
                type="radio"
                name={theme}
                bind:group={$current_theme}
                value={theme}
            />
            <label for={theme} style="margin-left: 0.5em; white-space:nowrap;">
                {theme.replace(/([A-Z])/g, (match) => ` ${match.toLowerCase()}`)}
            </label>
        </div>
    {/each}
</div>

<style>
    .theme_chooser {
        display: flex;
        flex-wrap: wrap;
        flex-direction: var(--flexDirection);
    }
</style>
