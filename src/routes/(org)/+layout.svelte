<script lang="ts">
    const org_css = {
        // sandyuraz: "https://sandyuraz.com/styles/org.css",
        "solarized-light":
            "https://thomasf.github.io/solarized-css/solarized-light.min.css",
        "solarized-dark":
            "https://thomasf.github.io/solarized-css/solarized-dark.min.css",
        orgcss: "https://gongzhitaao.org/orgcss/org.css",
    } as const;

    let current_theme: keyof typeof org_css | "default-minimal" =
        "solarized-dark";
    let post_width: number, screen_width: number, theme_chooser_width: number;
    let theme_chooser_on_the_right_side: boolean = true;
    $: side_bar_width = (screen_width - post_width) / 2;
    $: theme_chooser_on_the_right_side =
        side_bar_width > theme_chooser_width * 1.4;
</script>

<svelte:window bind:innerWidth={screen_width} />
<svelte:head>
    {#if current_theme != "default-minimal"}
        <link rel="stylesheet" type="text/css" href={org_css[current_theme]} />
    {/if}
</svelte:head>

<div
    class:theme_chooser_on_the_right_side
    style:right={theme_chooser_on_the_right_side
        ? `${(side_bar_width - theme_chooser_width) / 2}px`
        : null}
>
    <div style:display="flex" style:flex-direction="row">
        <h4 bind:clientWidth={theme_chooser_width} style:padding-right="0">
            Choose your eye candy
        </h4>
        <p />
    </div>
    <div
        class="theme_chooser"
        style:flex-direction={theme_chooser_on_the_right_side
            ? "column"
            : "row"}
    >
        {#each [...Object.keys(org_css), "default-minimal"] as theme}
            <div class="radio">
                <input
                    type="radio"
                    name={theme}
                    bind:group={current_theme}
                    value={theme}
                />
                <label for={theme}>
                    {theme}
                </label>
            </div>
        {/each}
    </div>
</div>
<div bind:clientWidth={post_width}>
    <slot />
</div>

<style>
    .theme_chooser_on_the_right_side {
        position: fixed;
        right: 2em;
        top: 2em;
        z-index: 1;
    }
    .theme_chooser {
        display: flex;
        flex-wrap: wrap;
    }
    .radio {
        padding: 0.5em;
    }
</style>
