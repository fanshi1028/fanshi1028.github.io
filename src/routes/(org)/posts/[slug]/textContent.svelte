<script lang="ts">
    import { slide } from "svelte/transition";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";
    import type { ChildNode } from "./types";

    export let textEle: ChildNode;
</script>

{#if textEle.type == "text"}
    {textEle.data}
{:else if textEle.type == "tag"}
    {#if textEle.name == "a"}
        <a
            href={textEle.attribs.href}
            class:dark:text-dark-academia-olive-drab={$isDarkAcademia}
            class:text-solarized-yellow={$isSolarized}
            transition:slide|global
        >
            {#each textEle.children as child}
                <svelte:self textEle={child} />
            {/each}
        </a>
    {:else if textEle.name == "span"}
        {@const isTODO = /TODO/.test(textEle.attribs.class)}
        {@const isDONE = /DONE/.test(textEle.attribs.class)}
        {@const isSecNum = /section-number-[\d]+/.test(textEle.attribs.class)}
        <span
            class:dark:text-solarized-dark-content-secondary={$isSolarized &&
                (isSecNum || isDONE)}
            class:text-solarized-content-secondary={$isSolarized &&
                (isSecNum || isDONE)}
            class:bg-solarized-cyan={isDONE && $isSolarized}
            class:bg-solarized-orange={isTODO && $isSolarized}
            class:text-solarized-content-emphasized={isTODO && $isSolarized}
            class:dark:text-solarized-dark-content-emphasized={isTODO &&
                $isSolarized}
            class="py-0.5 px-1.5 rounded-xl"
        >
            {#each textEle.children as child}
                <svelte:self textEle={child} />
            {/each}
        </span>
    {:else}
        <svelte:element
            this={textEle.name}
            class:text-dark-academia-charcoal-gray={$isDarkAcademia}
            class:dark:text-solarized-dark-content={$isSolarized}
            class:text-solarized-content={$isSolarized}
        >
            {#each textEle.children as child}
                <svelte:self textEle={child} />
            {/each}
        </svelte:element>
    {/if}
{/if}
