<script lang="ts">
    import TextContent from "./textContent.svelte";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";
    import type { DomElement } from "./types";

    export let listEle: DomElement;
</script>

<svelte:element
    this={listEle.name}
    class="list-inside marker:text-2xl"
    class:text-dark-academia-rosy-brown={$isDarkAcademia}
    class:marker:text-solarized-blue={$isSolarized}
    class:list-decimal={listEle.name == "ol"}
    class:list-disc={listEle.name == "ul"}
>
    {#each listEle.children as child}
        {#if child.type == "tag" && child.name == "li"}
            <li
                class:text-solarized-content-emphasized={$isSolarized}
                class:dark:text-solarized-dark-content-emphasized={$isSolarized}
            >
                {#each child.children as grandChild}
                    <TextContent textEle={grandChild} />
                {/each}
            </li>
        {/if}
    {/each}
</svelte:element>
