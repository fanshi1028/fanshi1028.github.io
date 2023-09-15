<script lang="ts">
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";
    import type { DomElement } from "./types";
    import OutlineText from "./outlineText.svelte";
    import TextContent from "./textContent.svelte";

    export let listEle: DomElement;
    export let level: number;

    $: items = listEle.children.reduce<DomElement[]>(
        (acc, ele) =>
            ele.type == "tag" && ele.name == "li" ? [...acc, ele] : acc,
        []
    );
</script>

{#if listEle.name == "ol" || listEle.name == "ul"}
    <svelte:element
        this={listEle.name}
        class="px-3 marker:text-2xl"
        class:text-dark-academia-rosy-brown={$isDarkAcademia}
        class:marker:text-solarized-blue={$isSolarized}
        class:list-decimal={listEle.name == "ol"}
        class:list-disc={listEle.name == "ul"}
    >
        {#each items as item}
            <li
                class="ml-6 text-content-emphasized dark:text-dark-content-emphasized"
            >
                {#each item.children as child}
                    {#if child.type == "text"}
                        {child.data}
                    {:else if child.type == "tag"}
                        {#if child.name == "div" && child.attribs.class == `outline-${level + 1}`}
                            <OutlineText ele={child} level={level + 1} />
                        {:else}
                            <TextContent textEle={child} />
                        {/if}
                    {/if}
                {/each}
            </li>
        {/each}
    </svelte:element>
{/if}
