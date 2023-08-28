<script lang="ts">
    import TextContent from "./textContent.svelte";
    import DescriptionList from "./descriptionList.svelte";
    import List from "./list.svelte";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";

    export let ele: DomElement;

    import "app-css";
    import type { DomElement } from "./types";
</script>

{#each ele.children as child}
    {#if child.type == "tag"}
        {#if child.attribs.class == "org-src-container"}
            {#each child.children as srcChild}
                {#if srcChild?.type == "tag" && srcChild.name == "pre"}
                    <!-- prettier-ignore -->
                    <pre class="px-3"><TextContent textEle={srcChild} /></pre>
                {/if}
            {/each}
        {:else if child.name == "dl" || child.name == "ul" || child.name == "ol"}
            <div class="px-3">
                <svelte:component
                    this={child.name == "dl" ? DescriptionList : List}
                    listEle={child}
                />
            </div>
        {:else if child.name == "p"}
            <p
                class:text-dark-academia-charcoal-gray={$isDarkAcademia}
                class:dark:text-dark-academia-grayish-blue={$isDarkAcademia}
                class:dark:text-solarized-dark-content={$isSolarized}
                class:text-solarized-content={$isSolarized}
                class="px-3 font-serif"
            >
                <TextContent textEle={child} />
            </p>
        {:else}
            <TextContent textEle={child} />
        {/if}
    {:else}
        <TextContent textEle={child} />
    {/if}
{/each}

