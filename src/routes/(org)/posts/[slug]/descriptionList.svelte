<script lang="ts">
    import { textContent } from "domutils";
    import type { DomElement } from "./types";

    export let listEle: DomElement;

    const [titleWithDescriptionList] = listEle.children.reduce<
        [
            { details: string; title: string; id: number }[],
            { details?: string; title?: string },
            number
        ]
    >(
        ([results, partial, nextId], child) => {
            const updated_partial =
                child.type == "tag"
                    ? child.name == "dt"
                        ? { ...partial, title: textContent(child) }
                        : child.name == "dd"
                        ? { ...partial, details: textContent(child) }
                        : partial
                    : partial;

            const { details, title } = updated_partial;
            return title && details
                ? [[...results, { title, details, id: nextId }], {}, nextId + 1]
                : [results, updated_partial, nextId];
        },
        [[], {}, 0]
    );

    let selectedId: number | null = null;

    $: currentDescription = titleWithDescriptionList.reduce<string | null>(
        (acc, { id, details }) => acc || (id == selectedId ? details : null),
        null
    );
</script>

<fieldset class="px-3 {selectedId === null ? 'pt-3' : 'pt-5'} ">
    {#each titleWithDescriptionList as { title, id }}
        <input
            type="radio"
            id={`${id}`}
            value={id == selectedId ? null : id}
            bind:group={selectedId}
        />
        <label
            class="group cursor-pointer list-none
                   text-content-emphasized dark:text-dark-content-emphasized
                   hover:text-solarized-cyan hover:font-semibold
                   rounded-t-full
                   {selectedId == id
                ? 'bg-background-highlight dark:dark:bg-dark-background-highlight px-6 pt-5 pb-4'
                : ''}
                   "
            for={`${id}`}
        >
            {title}
        </label>
    {/each}
</fieldset>
{#if currentDescription}
    <p
        class="bg-background-highlight dark:bg-dark-background-highlight
               text-content-emphasized dark:text-dark-content-emphasized

               p-4 mt-1 rounded-xl
               "
    >
        {currentDescription}
    </p>
{/if}

<style lang="postcss">
    /* details {
    transition: 0.15s background linear;
    } */
    input {
        opacity: 0;
    }
</style>
