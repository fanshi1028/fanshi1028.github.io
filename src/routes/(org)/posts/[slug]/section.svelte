<script lang="ts">
    import TextContent from "./textContent.svelte";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";

    export let displaylevels = [1, 2, 3, 4, 5];
    export let level: number;
    export let ele: DomElement;

    import "app-css";
    import type { DomElement } from "./types";
    import SectionText from "./sectionText.svelte";
</script>

{#if displaylevels.indexOf(level) != -1}
    <div class="grid row-auto gap-3">
        {#each ele.children as child}
            {#if child.type == "tag"}
                {#if child.name == `h1`}
                    <h1
                        class="text-5xl font-extrabold font-sans text-center py-4"
                        class:text-solarized-magenta={$isSolarized}
                        class:text-dark-academia-gray-olive={$isDarkAcademia}
                    >
                        {#each child.children as grandChild}
                            <TextContent textEle={grandChild} />
                        {/each}
                    </h1>
                {:else if child.name == `h${level}`}
                    <svelte:element
                        this={`h${level}`}
                        class:text-solarized-green={$isSolarized}
                        class:text-dark-academia-rosy-brown={$isDarkAcademia}
                        class:pt-5={level == 2}
                    >
                        {#each child.children as grandChild}
                            <TextContent textEle={grandChild} />
                        {/each}
                    </svelte:element>
                {:else if child.attribs.class == `outline-text-${level}`}
                    <SectionText ele={child} />
                {:else if child.attribs.class == `outline-${level + 1}`}
                    <!-- prettier-ignore -->
                    <div
                        class="px-7 pb-5"
                        class:rounded-xl={level == 1}
                        class:bg-dark-academia-charcoal-gray={level == 1 && $isDarkAcademia}
                        class:dark:bg-dark-academia-grayish-blue={level == 1 && $isDarkAcademia}
                        class:bg-solarized-background-highlight={level == 1 && $isSolarized}
                        class:dark:bg-solarized-dark-background-highlight={level == 1 && $isSolarized}
                        class:text-solarized-green={level >= 2 && isSolarized}
                    >
                        <svelte:self
                            {displaylevels}
                            level={level + 1}
                            ele={child}
                        />
                    </div>
                {/if}
            {/if}
        {/each}
    </div>
{/if}

<style lang="postcss">
    h2 {
        @apply text-4xl font-bold;
    }
    h3 {
        @apply text-3xl font-semibold;
    }
    h4 {
        @apply text-2xl font-normal;
    }
    h5 {
        @apply text-xl;
    }
</style>
