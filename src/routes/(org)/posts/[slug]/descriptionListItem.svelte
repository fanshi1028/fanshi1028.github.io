<script lang="ts">
    import { textContent } from "domutils";
    import type { DomElement } from "./types";
    import { isDarkAcademia, isSolarized } from "../../orgThemeChooser.svelte";

    export let dt: DomElement;
    export let dd: DomElement;

    export let currentHovered: number | null;

    export let itemId: number;

    export let ddHidden: "hidden" | "block" = "hidden";
</script>

<dt
    class:dark:text-solarized-dark-content-secondary={$isSolarized &&
        currentHovered !== null}
    class:dark:text-solarized-dark-content={$isSolarized &&
        currentHovered === null}
    class:text-solarized-content-secondary={$isSolarized &&
        currentHovered !== null}
    class:text-solarized-content={$isSolarized && currentHovered === null}
    on:mouseover={() => {
        ddHidden = "block";
        currentHovered = itemId;
    }}
    on:mouseleave={() => {
        ddHidden = "hidden";
        currentHovered = null;
    }}
>
    {textContent(dt)}
</dt>
<dd
    class:dark:text-solarized-dark-content-emphasized={$isSolarized}
    class="pl-3 {ddHidden}"
>
    {textContent(dd)}
</dd>
