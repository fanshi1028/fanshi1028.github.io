<script lang="ts" context="module">
    const current_theme = writable<OrgTheme>("defaultMinimal");

    export const readable_current_theme = {
        subscribe: current_theme.subscribe,
    };
</script>

<script lang="ts">
    import { writable } from "svelte/store";
    import { org_css, type OrgTheme } from "./orgThemes";
    import { slide } from "svelte/transition";
    export let slideDirection: "x" | "y";
</script>

<h4>Choose your eye candy!</h4>

<div style="display: flex; flex-wrap: wrap;" class="flex-row lg:flex-col">
    {#each [...Object.keys(org_css), "defaultMinimal"] as theme, idx (idx)}
        <div
            style="display: flex;"
            class="px-1 py-1"
            in:slide|global={{
                delay: idx * 100,
                axis: slideDirection,
            }}
        >
            <input
                type="radio"
                name={theme}
                bind:group={$current_theme}
                value={theme}
            />
            <label for={theme} style="white-space:nowrap;" class="ml-2">
                {theme.replace(
                    /([A-Z])/g,
                    (match) => ` ${match.toLowerCase()}`
                )}
            </label>
        </div>
    {/each}
</div>
