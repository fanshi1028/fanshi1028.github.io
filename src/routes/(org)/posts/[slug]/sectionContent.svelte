<script lang="ts">
    export let start = 1;
    export let depth = 5;
    export let level: number;
    export let ele: DomElement;

    import "app-css";
    import type { DomElement } from "./types";
    import ListLike from "./listLike.svelte";
    import OutlineText from "./outlineText.svelte";
    import TextContent from "./textContent.svelte";

    $: children = ele.children.reduce<DomElement[]>(
        (acc, ele) => (ele.type == "tag" ? [...acc, ele] : acc),
        []
    );
</script>

{#if level >= start && level < start + depth}
    {#each children as child}
        {#if child.name == `h${level}`}
            <!-- prettier-ignore -->
            <svelte:element
                this={`h${level}`}
                class="py-2 font-serif"

                class:pt-5={level == 2}
                class:text-4xl={level == 2}
                class:font-bold={level == 2}

                class:text-3xl={level == 3}
                class:font-semibold={level == 3}

                class:text-2xl={level == 4}
                class:font-normal={level == 4}

                class:text-xl={level == 5}
            >
                <TextContent textEle={child} />
            </svelte:element>
        {:else if child.attribs.class == `outline-text-${level}`}
            <OutlineText ele={child} {level} />
        {:else if child.attribs.class == `outline-${level + 1}`}
            <div class="pl-7 pb-2">
                <svelte:self {start} {depth} level={level + 1} ele={child} />
            </div>
        {:else if child.name == "ol"}
            <ListLike listEle={child} {level} />
        {/if}
    {/each}
{/if}

<!-- /* text-shadow: 1px 1px 2px theme(colors.solarized.violet); */ -->
