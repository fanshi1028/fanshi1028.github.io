<script lang="ts">
    import { org_css, type OrgTheme } from "./orgThemes";
    import { slide } from "svelte/transition";

    export let current_theme: OrgTheme;

    export let flexDirection: "row" | "column";

    export let read_only_min_width: number;
</script>

<h4
    bind:clientWidth={read_only_min_width}
    style="width: fit-content; padding: 0 0.5em;"
>
    Choose your eye candy!
</h4>

<div class="theme_chooser" style:--flexDirection={flexDirection}>
    {#each [...Object.keys(org_css), "default-minimal"] as theme, idx (idx)}
        <div
            style="padding-left: 1em; padding-bottom: 0.5em; display: flex"
            transition:slide|global={{
                duration: 700,
                delay: idx * 200,
                axis: flexDirection == "row" ? "x" : "y",
            }}
        >
            <input
                type="radio"
                name={theme}
                bind:group={current_theme}
                value={theme}
            />
            <label for={theme} style="margin-left: 0.5em; white-space:nowrap;">
                {theme}
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
