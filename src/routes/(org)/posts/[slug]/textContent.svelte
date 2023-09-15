<script lang="ts">
    import { slide } from "svelte/transition";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";
    import type { DomElement } from "./types";

    export let textEle: DomElement;
</script>

{#each textEle.children as child}
    {#if child.type == "text"}
        {child.data}
    {:else if child.type == "tag"}
        {#if child.name == "br"}
            <br />
        {:else if child.name == "a"}
            <a
                href={child.attribs.href}
                class:dark:text-dark-academia-olive-drab={$isDarkAcademia}
                class:text-solarized-yellow={$isSolarized}
                transition:slide|global
            >
                <svelte:self textEle={child} />
            </a>
        {:else if child.name == "span"}
            {@const isTODO = /TODO/.test(child.attribs.class)}
            {@const isDONE = /DONE/.test(child.attribs.class)}
            {@const isSecNum = /section-number-[\d]+/.test(child.attribs.class)}
            <span
                class:dark:text-dark-content-secondary={isSecNum || isDONE}
                class:text-content-secondary={isSecNum || isDONE}
                class:bg-solarized-cyan={isDONE && $isSolarized}
                class:bg-solarized-orange={isTODO && $isSolarized}
                class:text-content-emphasized={isTODO}
                class:dark:text-dark-content-emphasized={isTODO}
            >
                <svelte:self textEle={child} />
            </span>
        {:else}
            <svelte:element
                this={child.name}
                class="text-content dark:text-dark-content"
            >
                <svelte:self textEle={child} />
            </svelte:element>
        {/if}
    {/if}
{/each}

<style lang="postcss">
    span {
        @apply py-0.5 px-1.5 rounded-xl;
    }
</style>
