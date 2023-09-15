<script lang="ts">
    import { invalidate } from "$app/navigation";
    import { parseDocument } from "htmlparser2";
    import type { PageServerData } from "./$types";
    import { findOne, getAttributeValue } from "domutils";
    import SectionContent from "./sectionContent.svelte";
    import { current_theme, theme_colors } from "../../orgThemeChooser.svelte";
    import TextContent from "./textContent.svelte";

    export let data: PageServerData;

    $: dom = parseDocument(data.html);

    $: content = findOne(
        (ele) =>
            ele.name == "div" &&
            getAttributeValue(ele, "id") == "content" &&
            getAttributeValue(ele, "class") == "content",
        dom.children
    );

    $: title_color =
        $current_theme == "solarized"
            ? theme_colors.solarized.magenta
            : $current_theme == "dark-academia"
            ? theme_colors["dark-academia"]["gray-olive"].DEFAULT
            : null;

    $: section_title_color =
        $current_theme == "solarized"
            ? theme_colors.solarized.green
            : $current_theme == "dark-academia"
            ? theme_colors["dark-academia"]["rosy-brown"].DEFAULT
            : null;

    import.meta.hot?.on("org_post_update", invalidate);
</script>

{#if content}
    <div class="grid row-auto gap-3">
        {#each content.children as child}
            {#if child.type == "text"}
                {child.data}
            {:else if child.type == "tag"}
                {#if child.name == `h1`}
                    <h1
                        style:color={title_color}
                        class="font-serif text-5xl font-extrabold text-center py-4 whitespace-nowrap"
                    >
                        <TextContent textEle={child} />
                    </h1>
                {:else if child.attribs.class == `outline-2`}
                    <section
                        style:color={section_title_color}
                        class="px-7 pb-5 rounded-xl shadow-2xl shadow-solarized-violet/20 focus:ring-1 focus:ring-content-emphasized"
                    >
                        <SectionContent ele={child} level={2} />
                    </section>
                {/if}
            {/if}
        {/each}
    </div>
{/if}
{@html data.html}

<!-- /* text-shadow: 1px 1px 2px theme(colors.solarized.orange); */ -->

<!-- /* ring-1 ring-solarized-blue/60 */
     /* bg-background-highlight dark:bg-dark-background-highlight */ -->
