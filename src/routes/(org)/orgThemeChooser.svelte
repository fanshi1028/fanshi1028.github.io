<script lang="ts" context="module">
    import tailwindConfig from "tailwind-config";
    const {
        theme: { colors },
    } = tailwindConfig;

    type ColorSchemes = keyof typeof colors;

    const colorSchemes = Object.keys(colors) as ColorSchemes[]; // HACK

    const _current_theme = writable<ColorSchemes>("solarized");

    export const current_theme: Readable<ColorSchemes> = {
        subscribe: _current_theme.subscribe,
    };

    export const isDarkAcademia: Readable<boolean> = derived(
        current_theme,
        ($t) => $t == "dark-academia"
    );
    export const isSolarized: Readable<boolean> = derived(
        current_theme,
        ($t) => $t == "solarized"
    );
</script>

<script lang="ts">
    import { writable, derived, type Readable } from "svelte/store";
    import { slide } from "svelte/transition";
    export let slideDirection: "x" | "y";
</script>

<h4 class="font-extrabold text-accent-luxury p-2 whitespace-nowrap">
    Choose your eye candy!
</h4>

<div
    style="display: flex; flex-wrap: wrap;"
    class="flex-row lg:flex-col lg:rounded lg:p-2"
>
    {#each colorSchemes as theme, idx (idx)}
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
                bind:group={$_current_theme}
                value={theme}
            />
            <label
                for={theme}
                style="white-space:nowrap;"
                class="ml-2 text-accent-tertiary"
            >
                {theme.replace(
                    /([A-Z])/g,
                    (match) => ` ${match.toLowerCase()}`
                )}
            </label>
        </div>
    {/each}
</div>
