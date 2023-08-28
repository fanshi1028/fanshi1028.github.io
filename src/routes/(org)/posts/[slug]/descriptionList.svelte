<script lang="ts">
    import DescriptionListItem from "./descriptionListItem.svelte";
    import type { DomElement } from "./types";

    export let listEle: DomElement;

    let currentHovered: number | null = null;

    const [dtddList] = listEle.children.reduce<
        [
            { dd: DomElement; dt: DomElement }[],
            { dd?: DomElement; dt?: DomElement }
        ]
    >(
        ([dtdds, partialDtdd], child) => {
            const updated_partial =
                child.type == "tag"
                    ? child.name == "dt"
                        ? { ...partialDtdd, dt: child }
                        : child.name == "dd"
                        ? { ...partialDtdd, dd: child }
                        : partialDtdd
                    : partialDtdd;

            const { dd, dt } = updated_partial;
            return dt && dd
                ? [[...dtdds, { dt, dd }], {}]
                : [dtdds, updated_partial];
        },
        [[], {}]
    );
    $: console.log(dtddList);
</script>

<dl>
    {#each dtddList as dddt, itemId (itemId)}
        <DescriptionListItem {...dddt} {itemId} bind:currentHovered />
    {/each}
</dl>
