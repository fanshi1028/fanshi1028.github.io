<script lang="ts">
    import TextContent from "./textContent.svelte";

    import DescriptionList from "./descriptionList.svelte";

    export let ele: DomElement;
    export let level: number;

    import "app-css";
    import type { DomElement } from "./types";
    import { textContent } from "domutils";
    import ListLike from "./listLike.svelte";
</script>

{#each ele.children as child}
    {#if child.type == "text"}
        {child.data}
    {:else if child.type == "tag"}
        {#if child.name == "ul" || child.name == "ol"}
            <ListLike listEle={child} {level} />
        {:else if child.name == "dl"}
            <DescriptionList listEle={child} />
        {:else if child.attribs.class == "org-src-container"}
            {#each child.children as srcChild}
                {#if srcChild?.type == "tag" && srcChild.name == "pre"}
                    <pre class="px-3">{textContent(srcChild)}</pre>
                {/if}
            {/each}
        {:else if child.name == "br"}
            <br />
        {:else if child.name == "p"}
            <p
                class="up-3 text-content dark:text-dark-content
                      {level == 2
                    ? `first-of-type:first-letter:text-5xl first-of-type:first-letter:font-semibold first-of-type:first-letter:float-left first-of-type:fist-letter:mr-3
                       first-of-type:first-line:uppercase first-of-type:first-line:tracking-widest`
                    : ''}"
            >
                <TextContent textEle={child} />
            </p>
        {:else}
            <TextContent textEle={child} />
        {/if}
    {/if}
{/each}

<style lang="postcss">
</style>
