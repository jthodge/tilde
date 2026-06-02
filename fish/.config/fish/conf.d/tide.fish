# Tide prompt configuration — Classic preset adapted for Spaceship-equivalent feel.
# Customizations vs. stock Classic:
#   - context always displayed (matches Spaceship SPACESHIP_USER_SHOW=always + _HOST_SHOW=always)
#   - time segment enabled on the right (matches SPACESHIP_TIME_SHOW=true)
#   - cmd_duration threshold lowered from 3000ms to 1000ms

# Color anchors
set -g _tide_color_dark_blue 0087AF
set -g _tide_color_dark_green 5FAF00
set -g _tide_color_gold AF8700
set -g _tide_color_green 5FD700
set -g _tide_color_light_blue 00AFFF

# Layout
set -g tide_left_prompt_items pwd git newline
set -g tide_right_prompt_items status cmd_duration context jobs direnv node python rustc go kubectl aws terraform time
set -g tide_left_prompt_frame_enabled true
set -g tide_right_prompt_frame_enabled true
set -g tide_left_prompt_prefix ''
set -g tide_left_prompt_suffix
set -g tide_right_prompt_prefix
set -g tide_right_prompt_suffix ''
set -g tide_left_prompt_separator_diff_color
set -g tide_left_prompt_separator_same_color
set -g tide_right_prompt_separator_diff_color
set -g tide_right_prompt_separator_same_color
set -g tide_prompt_add_newline_before true
set -g tide_prompt_color_frame_and_connection 6C6C6C
set -g tide_prompt_color_separator_same_color 949494
set -g tide_prompt_min_cols 34
set -g tide_prompt_pad_items true
set -g tide_prompt_transient_enabled false

# Character
set -g tide_character_color $_tide_color_green
set -g tide_character_color_failure FF0000

# Context (always show user@host — Spaceship parity)
set -g tide_context_always_display true
set -g tide_context_bg_color 444444
set -g tide_context_color_default D7AF87
set -g tide_context_color_root $_tide_color_gold
set -g tide_context_color_ssh D7AF87
set -g tide_context_hostname_parts 1

# pwd
set -g tide_pwd_bg_color 444444
set -g tide_pwd_color_anchors $_tide_color_light_blue
set -g tide_pwd_color_dirs $_tide_color_dark_blue
set -g tide_pwd_color_truncated_dirs 8787AF
set -g tide_pwd_markers .bzr .citc .git .hg .node-version .python-version .ruby-version .shorten_folder_marker .svn .terraform bun.lockb Cargo.toml composer.json CVS go.mod package.json build.zig

# git
set -g tide_git_bg_color 444444
set -g tide_git_bg_color_unstable 444444
set -g tide_git_bg_color_urgent 444444
set -g tide_git_color_branch $_tide_color_green
set -g tide_git_color_conflicted FF0000
set -g tide_git_color_dirty $_tide_color_gold
set -g tide_git_color_operation FF0000
set -g tide_git_color_staged $_tide_color_gold
set -g tide_git_color_stash $_tide_color_green
set -g tide_git_color_untracked $_tide_color_light_blue
set -g tide_git_color_upstream $_tide_color_green
set -g tide_git_truncation_length 24
set -g tide_git_truncation_strategy

# status
set -g tide_status_bg_color 444444
set -g tide_status_bg_color_failure 444444
set -g tide_status_color $_tide_color_dark_green
set -g tide_status_color_failure D70000

# cmd_duration (Spaceship shows exec time > 1s by default)
set -g tide_cmd_duration_bg_color 444444
set -g tide_cmd_duration_color 87875F
set -g tide_cmd_duration_decimals 0
set -g tide_cmd_duration_threshold 1000

# jobs
set -g tide_jobs_bg_color 444444
set -g tide_jobs_color $_tide_color_dark_green
set -g tide_jobs_number_threshold 1000

# time
set -g tide_time_bg_color 444444
set -g tide_time_color 5F8787
set -g tide_time_format %T

# Language / runtime segments
set -g tide_node_bg_color 444444
set -g tide_node_color 44883E
set -g tide_python_bg_color 444444
set -g tide_python_color 00AFAF
set -g tide_rustc_bg_color 444444
set -g tide_rustc_color F74C00
set -g tide_go_bg_color 444444
set -g tide_go_color 00ACD7
set -g tide_ruby_bg_color 444444
set -g tide_ruby_color B31209
set -g tide_java_bg_color 444444
set -g tide_java_color ED8B00
set -g tide_php_bg_color 444444
set -g tide_php_color 617CBE
set -g tide_elixir_bg_color 444444
set -g tide_elixir_color 4E2A8E
set -g tide_crystal_bg_color 444444
set -g tide_crystal_color FFFFFF
set -g tide_zig_bg_color 444444
set -g tide_zig_color F7A41D
set -g tide_bun_bg_color 14151A
set -g tide_bun_color FBF0DF

# Infrastructure / context segments
set -g tide_kubectl_bg_color 444444
set -g tide_kubectl_color 326CE5
set -g tide_aws_bg_color 444444
set -g tide_aws_color FF9900
set -g tide_gcloud_bg_color 444444
set -g tide_gcloud_color 4285F4
set -g tide_terraform_bg_color 444444
set -g tide_terraform_color 844FBA
set -g tide_pulumi_bg_color 444444
set -g tide_pulumi_color F7BF2A
set -g tide_docker_bg_color 444444
set -g tide_docker_color 2496ED
set -g tide_docker_default_contexts default colima
set -g tide_nix_shell_bg_color 444444
set -g tide_nix_shell_color 7EBAE4

# Sandbox / shell-level
set -g tide_distrobox_bg_color 444444
set -g tide_distrobox_color FF00FF
set -g tide_toolbox_bg_color 444444
set -g tide_toolbox_color 613583
set -g tide_direnv_bg_color 444444
set -g tide_direnv_bg_color_denied 444444
set -g tide_direnv_color $_tide_color_gold
set -g tide_direnv_color_denied FF0000
set -g tide_private_mode_bg_color 444444
set -g tide_private_mode_color FFFFFF
set -g tide_os_bg_color 444444
set -g tide_os_color EEEEEE
set -g tide_shlvl_bg_color 444444
set -g tide_shlvl_color d78700
set -g tide_shlvl_threshold 1

# vi-mode (off by default but configured)
set -g tide_vi_mode_bg_color_default 444444
set -g tide_vi_mode_bg_color_insert 444444
set -g tide_vi_mode_bg_color_replace 444444
set -g tide_vi_mode_bg_color_visual 444444
set -g tide_vi_mode_color_default 949494
set -g tide_vi_mode_color_insert 87AFAF
set -g tide_vi_mode_color_replace 87AF87
set -g tide_vi_mode_color_visual FF8700
