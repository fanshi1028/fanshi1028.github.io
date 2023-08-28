<script lang="ts">
    import { invalidate } from "$app/navigation";
    import { parseDocument } from "htmlparser2";
    import type { PageServerData } from "./$types";
    import { findOne, getAttributeValue } from "domutils";
    import Section from "./section.svelte";
    export let data: PageServerData;

    $: dom = parseDocument(data.html);

    $: content = findOne(
        (ele) =>
            ele.name == "div" &&
            getAttributeValue(ele, "id") == "content" &&
            getAttributeValue(ele, "class") == "content",
        dom.children
    );

    import.meta.hot?.on("org_post_update", invalidate);
</script>

{#if content} <Section ele={content} level={1} /> {/if}
{@html data.html}
