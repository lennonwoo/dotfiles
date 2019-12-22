"""Mackup.

Keep your application settings in sync.
Copyright (C) 2013-2015 Laurent Raufaste <http://glop.org/>

Usage:
  mackup list
  mackup [options] backup
  mackup [options] restore
  mackup [options] uninstall
  mackup (-h | --help)
  mackup --version

Options:
  -h --help     Show this screen.
  -f --force    Force every question asked to be answered with "Yes".
  -n --dry-run  Show steps without executing.
  -v --verbose  Show additional details.
  --version     Show version.

Modes of action:
 1. list: display a list of all supported applications.
 2. backup: sync your conf files to your synced storage, use this the 1st time
    you use Mackup. (Note that by default this will sync private keys used by
    GnuPG.)
 3. restore: link the conf files already in your synced storage on your system,
    use it on any new system you use.
 4. uninstall: reset everything as it was before using Mackup.

By default, Mackup syncs all application data (except for private keys) via
Dropbox, but may be configured to exclude applications or use a different
backend with a .mackup.cfg file.

See https://github.com/lra/mackup/tree/master/doc for more information.

"""
import os
import os.path
import shutil
import tempfile
import base64
import platform
import shutil
import stat
import subprocess
import sys
import sqlite3
import argparse
from docopt import docopt

from docopt import docopt
from six.moves import input

try:
    import configparser
except ImportError:
    import ConfigParser as configparser


# Current version
VERSION = '0.8.24'

# Support platforms
PLATFORM_DARWIN = 'Darwin'
PLATFORM_LINUX = 'Linux'

# Directory containing the application configs
APPS_DIR = 'applications'

# Mackup application name
MACKUP_APP_NAME = 'mackup'

# Default Mackup backup path where it stores its files in Dropbox
MACKUP_BACKUP_PATH = 'Mackup'

# Mackup config file
MACKUP_CONFIG_FILE = '.mackup.cfg'

# Directory that can contains user defined app configs
CUSTOM_APPS_DIR = '.mackup'

# Supported engines
ENGINE_BOX = 'box'
ENGINE_COPY = 'copy'
ENGINE_DROPBOX = 'dropbox'
ENGINE_FS = 'file_system'
ENGINE_GDRIVE = 'google_drive'
ENGINE_ICLOUD = 'icloud'

# Flag that controls how user confirmation works.
# If True, the user wants to say "yes" to everything.
FORCE_YES = False


def confirm(question):
    """
    Ask the user if he really want something to happen.

    Args:
        question(str): What can happen

    Returns:
        (boolean): Confirmed or not
    """
    if FORCE_YES:
        return True

    while True:
        answer = input(question + ' <Yes|No>').lower()

        if answer == 'yes' or answer == 'y':
            confirmed = True
            break
        if answer == 'no' or answer == 'n':
            confirmed = False
            break

    return confirmed


def delete(filepath):
    """
    Delete the given file, directory or link.

    It Should support undelete later on.

    Args:
        filepath (str): Absolute full path to a file. e.g. /path/to/file
    """
    # Some files have ACLs, let's remove them recursively
    remove_acl(filepath)

    # Some files have immutable attributes, let's remove them recursively
    remove_immutable_attribute(filepath)

    # Finally remove the files and folders
    if os.path.isfile(filepath) or os.path.islink(filepath):
        os.remove(filepath)
    elif os.path.isdir(filepath):
        shutil.rmtree(filepath)


def copy(src, dst):
    """
    Copy a file or a folder (recursively) from src to dst.

    For simplicity sake, both src and dst must be absolute path and must
    include the filename of the file or folder.
    Also do not include any trailing slash.

    e.g. copy('/path/to/src_file', '/path/to/dst_file')
    or copy('/path/to/src_folder', '/path/to/dst_folder')

    But not: copy('/path/to/src_file', 'path/to/')
    or copy('/path/to/src_folder/', '/path/to/dst_folder')

    Args:
        src (str): Source file or folder
        dst (str): Destination file or folder
    """
    assert isinstance(src, str)
    assert os.path.exists(src)
    assert isinstance(dst, str)

    # Create the path to the dst file if it does not exists
    abs_path = os.path.dirname(os.path.abspath(dst))
    if not os.path.isdir(abs_path):
        os.makedirs(abs_path)

    # We need to copy a single file
    if os.path.isfile(src):
        # Copy the src file to dst
        shutil.copy(src, dst)

    # We need to copy a whole folder
    elif os.path.isdir(src):
        shutil.copytree(src, dst)

    # What the heck is this ?
    else:
        raise ValueError("Unsupported file: {}".format(src))

    # Set the good mode to the file or folder recursively
    chmod(dst)


def link(target, link_to):
    """
    Create a link to a target file or a folder.

    For simplicity sake, both target and link_to must be absolute path and must
    include the filename of the file or folder.
    Also do not include any trailing slash.

    e.g. link('/path/to/file', '/path/to/link')

    But not: link('/path/to/file', 'path/to/')
    or link('/path/to/folder/', '/path/to/link')

    Args:
        target (str): file or folder the link will point to
        link_to (str): Link to create
    """
    assert isinstance(target, str)
    assert os.path.exists(target)
    assert isinstance(link_to, str)

    # Create the path to the link if it does not exists
    abs_path = os.path.dirname(os.path.abspath(link_to))
    if not os.path.isdir(abs_path):
        os.makedirs(abs_path)

    # Make sure the file or folder recursively has the good mode
    chmod(target)

    # Create the link to target
    os.symlink(target, link_to)


def chmod(target):
    """
    Recursively set the chmod for files to 0600 and 0700 for folders.

    It's ok unless we need something more specific.

    Args:
        target (str): Root file or folder
    """
    assert isinstance(target, str)
    assert os.path.exists(target)

    file_mode = stat.S_IRUSR | stat.S_IWUSR
    folder_mode = stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR

    # Remove the immutable attribute recursively if there is one
    remove_immutable_attribute(target)

    if os.path.isfile(target):
        os.chmod(target, file_mode)

    elif os.path.isdir(target):
        # chmod the root item
        os.chmod(target, folder_mode)

        # chmod recursively in the folder it it's one
        for root, dirs, files in os.walk(target):
            for cur_dir in dirs:
                os.chmod(os.path.join(root, cur_dir), folder_mode)
            for cur_file in files:
                os.chmod(os.path.join(root, cur_file), file_mode)

    else:
        raise ValueError("Unsupported file type: {}".format(target))


def error(message):
    """
    Throw an error with the given message and immediately quit.

    Args:
        message(str): The message to display.
    """
    fail = '\033[91m'
    end = '\033[0m'
    sys.exit(fail + "Error: {}".format(message) + end)


def get_dropbox_folder_location():
    """
    Try to locate the Dropbox folder.

    Returns:
        (str) Full path to the current Dropbox folder
    """
    host_db_path = os.path.join(os.environ['HOME'], '.dropbox/host.db')
    try:
        with open(host_db_path, 'r') as f_hostdb:
            data = f_hostdb.read().split()
    except IOError:
        error("Unable to find your Dropbox install =(")
    dropbox_home = base64.b64decode(data[1]).decode()

    return dropbox_home


def get_google_drive_folder_location():
    """
    Try to locate the Google Drive folder.

    Returns:
        (str) Full path to the current Google Drive folder
    """
    gdrive_db_path = 'Library/Application Support/Google/Drive/sync_config.db'
    yosemite_gdrive_db_path = ('Library/Application Support/Google/Drive/'
                               'user_default/sync_config.db')
    yosemite_gdrive_db = os.path.join(os.environ['HOME'],
                                      yosemite_gdrive_db_path)
    if os.path.isfile(yosemite_gdrive_db):
        gdrive_db_path = yosemite_gdrive_db

    googledrive_home = None

    gdrive_db = os.path.join(os.environ['HOME'], gdrive_db_path)
    if os.path.isfile(gdrive_db):
        con = sqlite3.connect(gdrive_db)
        if con:
            cur = con.cursor()
            query = ("SELECT data_value "
                     "FROM data "
                     "WHERE entry_key = 'local_sync_root_path';")
            cur.execute(query)
            data = cur.fetchone()
            googledrive_home = str(data[0])
            con.close()

    if not googledrive_home:
        error("Unable to find your Google Drive install =(")

    return googledrive_home


def get_box_folder_location():
    """
    Try to locate the Box folder.

    Returns:
        (str) Full path to the current Box folder
    """
    box_prefs_path = ('Library/Application Support/Box/Box Sync/'
                      'sync_root_folder.txt')
    box_home = None

    box_prefs = os.path.join(os.environ['HOME'], box_prefs_path)
    try:
        with open(box_prefs, 'r') as sync_path:
            data = sync_path.read()
            box_home = data
    except IOError:
        error("Unable to find your Box prefs =(")

    return box_home


def get_copy_folder_location():
    """
    Try to locate the Copy folder.

    Returns:
        (str) Full path to the current Copy folder
    """
    copy_settings_path = 'Library/Application Support/Copy Agent/config.db'
    copy_home = None

    copy_settings = os.path.join(os.environ['HOME'], copy_settings_path)

    if os.path.isfile(copy_settings):
        database = sqlite3.connect(copy_settings)
        if database:
            cur = database.cursor()
            query = ("SELECT value "
                     "FROM config2 "
                     "WHERE option = 'csmRootPath';")
            cur.execute(query)
            data = cur.fetchone()
            copy_home = str(data[0])
            cur.close()

    if not copy_home:
        error("Unable to find your Copy install =(")

    return copy_home


def get_icloud_folder_location():
    """
    Try to locate the iCloud Drive folder.

    Returns:
        (str) Full path to the iCloud Drive folder.
    """
    yosemite_icloud_path = '~/Library/Mobile Documents/com~apple~CloudDocs/'

    icloud_home = os.path.expanduser(yosemite_icloud_path)

    if not os.path.isdir(icloud_home):
        error('Unable to find your iCloud Drive =(')

    return str(icloud_home)


def is_process_running(process_name):
    """
    Check if a process with the given name is running.

    Args:
        (str): Process name, e.g. "Sublime Text"

    Returns:
        (bool): True if the process is running
    """
    is_running = False

    # On systems with pgrep, check if the given process is running
    if os.path.isfile('/usr/bin/pgrep'):
        dev_null = open(os.devnull, 'wb')
        returncode = subprocess.call(['/usr/bin/pgrep', process_name],
                                     stdout=dev_null)
        is_running = bool(returncode == 0)

    return is_running


def remove_acl(path):
    """
    Remove the ACL of the file or folder located on the given path.

    Also remove the ACL of any file and folder below the given one,
    recursively.

    Args:
        path (str): Path to the file or folder to remove the ACL for,
                    recursively.
    """
    # Some files have ACLs, let's remove them recursively
    if (platform.system() == PLATFORM_DARWIN and
            os.path.isfile('/bin/chmod')):
        subprocess.call(['/bin/chmod', '-R', '-N', path])
    elif ((platform.system() == PLATFORM_LINUX) and
            os.path.isfile('/bin/setfacl')):
        subprocess.call(['/bin/setfacl', '-R', '-b', path])


def remove_immutable_attribute(path):
    """
    Remove the immutable attribute of the given path.

    Remove the immutable attribute of the file or folder located on the given
    path. Also remove the immutable attribute of any file and folder below the
    given one, recursively.

    Args:
        path (str): Path to the file or folder to remove the immutable
                    attribute for, recursively.
    """
    # Some files have ACLs, let's remove them recursively
    if ((platform.system() == PLATFORM_DARWIN) and
            os.path.isfile('/usr/bin/chflags')):
        subprocess.call(['/usr/bin/chflags', '-R', 'nouchg', path])
    elif (platform.system() == PLATFORM_LINUX and
            os.path.isfile('/usr/bin/chattr')):
        subprocess.call(['/usr/bin/chattr', '-R', '-i', path])


def can_file_be_synced_on_current_platform(path):
    """
    Check if the given path can be synced locally.

    Check if it makes sense to sync the file at the given path on the current
    platform.
    For now we don't sync any file in the ~/Library folder on GNU/Linux.
    There might be other exceptions in the future.

    Args:
        (str): Path to the file or folder to check. If relative, prepend it
               with the home folder.
               'abc' becomes '~/abc'
               '/def' stays '/def'

    Returns:
        (bool): True if given file can be synced
    """
    can_be_synced = True

    # If the given path is relative, prepend home
    fullpath = os.path.join(os.environ['HOME'], path)

    # Compute the ~/Library path on OS X
    # End it with a slash because we are looking for this specific folder and
    # not any file/folder named LibrarySomething
    library_path = os.path.join(os.environ['HOME'], 'Library/')

    if platform.system() == PLATFORM_LINUX:
        if fullpath.startswith(library_path):
            can_be_synced = False

    return can_be_synced


class ApplicationProfile(object):

    """Instantiate this class with application specific data."""

    def __init__(self, mackup, files, dry_run, verbose):
        """
        Create an ApplicationProfile instance.

        Args:
            mackup (Mackup)
            files (list)
        """
        assert isinstance(mackup, Mackup)
        assert isinstance(files, set)

        self.mackup = mackup
        self.files = list(files)
        self.dry_run = dry_run
        self.verbose = verbose

    def getFilepaths(self, filename):
        """
        Get home and mackup filepaths for given file

        Args:
            filename (str)

        Returns:
            home_filepath, mackup_filepath (str, str)
        """
        return (os.path.join(os.environ['HOME'], filename),
                os.path.join(self.mackup.mackup_folder, filename))

    def backup(self):
        """
        Backup the application config files.

        Algorithm:
            if exists home/file
              if home/file is a real file
                if exists mackup/file
                  are you sure ?
                  if sure
                    rm mackup/file
                    mv home/file mackup/file
                    link mackup/file home/file
                else
                  mv home/file mackup/file
                  link mackup/file home/file
        """
        # For each file used by the application
        for filename in self.files:
            (home_filepath, mackup_filepath) = self.getFilepaths(filename)

            # If the file exists and is not already a link pointing to Mackup
            if ((os.path.isfile(home_filepath) or
                 os.path.isdir(home_filepath)) and
                not (os.path.islink(home_filepath) and
                     (os.path.isfile(mackup_filepath) or
                      os.path.isdir(mackup_filepath)) and
                     os.path.samefile(home_filepath,
                                      mackup_filepath))):

                if self.verbose:
                    print("Backing up\n  {}\n  to\n  {} ..."
                          .format(home_filepath, mackup_filepath))
                else:
                    print("Backing up {} ...".format(filename))

                if self.dry_run:
                    continue

                # Check if we already have a backup
                if os.path.exists(mackup_filepath):

                    # Name it right
                    if os.path.isfile(mackup_filepath):
                        file_type = 'file'
                    elif os.path.isdir(mackup_filepath):
                        file_type = 'folder'
                    elif os.path.islink(mackup_filepath):
                        file_type = 'link'
                    else:
                        raise ValueError("Unsupported file: {}"
                                         .format(mackup_filepath))

                    # Ask the user if he really want to replace it
                    if confirm("A {} named {} already exists in the"
                                     " backup.\nAre you sure that you want to"
                                     " replace it ?"
                                     .format(file_type, mackup_filepath)):
                        # Delete the file in Mackup
                        delete(mackup_filepath)
                        # Copy the file
                        copy(home_filepath, mackup_filepath)
                        # Delete the file in the home
                        delete(home_filepath)
                        # Link the backuped file to its original place
                        link(mackup_filepath, home_filepath)
                else:
                    # Copy the file
                    copy(home_filepath, mackup_filepath)
                    # Delete the file in the home
                    delete(home_filepath)
                    # Link the backuped file to its original place
                    link(mackup_filepath, home_filepath)
            elif self.verbose:
                if os.path.exists(home_filepath):
                    print("Doing nothing\n  {}\n  "
                          "is already backed up to\n  {}"
                          .format(home_filepath, mackup_filepath))
                elif os.path.islink(home_filepath):
                    print("Doing nothing\n  {}\n  "
                          "is a broken link, you might want to fix it."
                          .format(home_filepath))
                else:
                    print("Doing nothing\n  {}\n  does not exist"
                          .format(home_filepath))

    def restore(self):
        """
        Restore the application config files.

        Algorithm:
            if exists mackup/file
              if exists home/file
                are you sure ?
                if sure
                  rm home/file
                  link mackup/file home/file
              else
                link mackup/file home/file
        """
        # For each file used by the application
        for filename in self.files:
            (home_filepath, mackup_filepath) = self.getFilepaths(filename)

            # If the file exists and is not already pointing to the mackup file
            # and the folder makes sense on the current platform (Don't sync
            # any subfolder of ~/Library on GNU/Linux)
            file_or_dir_exists = (os.path.isfile(mackup_filepath) or
                                  os.path.isdir(mackup_filepath))
            pointing_to_mackup = (os.path.islink(home_filepath) and
                                  os.path.exists(mackup_filepath) and
                                  os.path.samefile(mackup_filepath,
                                                   home_filepath))
            supported = can_file_be_synced_on_current_platform(filename)

            if file_or_dir_exists and not pointing_to_mackup and supported:
                if self.verbose:
                    print("Restoring\n  linking {}\n  to      {} ..."
                          .format(home_filepath, mackup_filepath))
                else:
                    print("Restoring {} ...".format(filename))

                if self.dry_run:
                    continue

                # Check if there is already a file in the home folder
                if os.path.exists(home_filepath):
                    # Name it right
                    if os.path.isfile(home_filepath):
                        file_type = 'file'
                    elif os.path.isdir(home_filepath):
                        file_type = 'folder'
                    elif os.path.islink(home_filepath):
                        file_type = 'link'
                    else:
                        raise ValueError("Unsupported file: {}"
                                         .format(mackup_filepath))

                    if confirm("You already have a {} named {} in your"
                                     " home.\nDo you want to replace it with"
                                     " your backup ?"
                                     .format(file_type, filename)):
                        delete(home_filepath)
                        link(mackup_filepath, home_filepath)
                else:
                    link(mackup_filepath, home_filepath)
            elif self.verbose:
                if os.path.exists(home_filepath):
                    print("Doing nothing\n  {}\n  already linked by\n  {}"
                          .format(mackup_filepath, home_filepath))
                elif os.path.islink(home_filepath):
                    print("Doing nothing\n  {}\n  "
                          "is a broken link, you might want to fix it."
                          .format(home_filepath))
                else:
                    print("Doing nothing\n  {}\n  does not exist"
                          .format(mackup_filepath))

    def uninstall(self):
        """
        Uninstall Mackup.

        Restore any file where it was before the 1st Mackup backup.

        Algorithm:
            for each file in config
                if mackup/file exists
                    if home/file exists
                        delete home/file
                    copy mackup/file home/file
            delete the mackup folder
            print how to delete mackup
        """
        # For each file used by the application
        for filename in self.files:
            (home_filepath, mackup_filepath) = self.getFilepaths(filename)

            # If the mackup file exists
            if (os.path.isfile(mackup_filepath) or
                    os.path.isdir(mackup_filepath)):
                # Check if there is a corresponding file in the home folder
                if os.path.exists(home_filepath):
                    if self.verbose:
                        print("Reverting {}\n  at {} ..."
                              .format(mackup_filepath, home_filepath))
                    else:
                        print("Reverting {} ...".format(filename))

                    if self.dry_run:
                        continue

                    # If there is, delete it as we are gonna copy the Dropbox
                    # one there
                    delete(home_filepath)

                    # Copy the Dropbox file to the home folder
                    copy(mackup_filepath, home_filepath)
            elif self.verbose:
                print("Doing nothing, {} does not exist"
                      .format(mackup_filepath))


class ApplicationsDatabase(object):

    """Database containing all the configured applications."""

    def __init__(self):
        """Create a ApplicationsDatabase instance."""
        # Build the dict that will contain the properties of each application
        self.apps = dict()

        for config_file in ApplicationsDatabase.get_config_files():
            config = configparser.ConfigParser(allow_no_value=True)

            # Needed to not lowercase the configuration_files in the ini files
            config.optionxform = str

            if config.read(config_file):
                # Get the filename without the directory name
                filename = os.path.basename(config_file)
                # The app name is the cfg filename with the extension
                app_name = filename[:-len('.cfg')]

                # Start building a dict for this app
                self.apps[app_name] = dict()

                # Add the fancy name for the app, for display purpose
                app_pretty_name = config.get('application', 'name')
                self.apps[app_name]['name'] = app_pretty_name

                # Add the configuration files to sync
                self.apps[app_name]['configuration_files'] = set()
                if config.has_section('configuration_files'):
                    for path in config.options('configuration_files'):
                        if path.startswith('/'):
                            raise ValueError('Unsupported absolute path: {}'
                                             .format(path))
                        self.apps[app_name]['configuration_files'].add(path)

                # Add the XDG configuration files to sync
                home = os.path.expanduser('~/')
                failobj = "{}.config".format(home)
                xdg_config_home = os.environ.get('XDG_CONFIG_HOME', failobj)
                if not xdg_config_home.startswith(home):
                    raise ValueError('$XDG_CONFIG_HOME: {} must be '
                                     'somewhere within your home '
                                     'directory: {}'
                                     .format(xdg_config_home, home))
                if config.has_section('xdg_configuration_files'):
                    for path in config.options('xdg_configuration_files'):
                        if path.startswith('/'):
                            raise ValueError('Unsupported absolute path: '
                                             '{}'
                                             .format(path))
                        path = os.path.join(xdg_config_home, path)
                        path = path.replace(home, '')
                        (self.apps[app_name]['configuration_files']
                            .add(path))

    @staticmethod
    def get_config_files():
        """
        Return the application configuration files.

        Return a list of configuration files describing the apps supported by
        Mackup. The files return are absolute full path to those files.
        e.g. /usr/lib/mackup/applications/bash.cfg

        Only one config file per application should be returned, custom config
        having a priority over stock config.

        Returns:
            set of strings.
        """
        custom_apps_dir = os.path.join(os.environ['HOME'], CUSTOM_APPS_DIR)

        # List of stock application config files
        config_files = set()

        # Temp list of user added app config file names
        custom_files = set()

        # Get the list of custom application config files first
        if os.path.isdir(custom_apps_dir):
            for filename in os.listdir(custom_apps_dir):
                if filename.endswith('.cfg'):
                    config_files.add(os.path.join(custom_apps_dir,
                                                  filename))
                    # Also add it to the set of custom apps, so that we don't
                    # add the stock config for the same app too
                    custom_files.add(filename)

        return config_files

    def get_name(self, name):
        """
        Return the fancy name of an application.

        Args:
            name (str)

        Returns:
            str
        """
        return self.apps[name]['name']

    def get_files(self, name):
        """
        Return the list of config files of an application.

        Args:
            name (str)

        Returns:
            set of str.
        """
        return self.apps[name]['configuration_files']

    def get_app_names(self):
        """
        Return application names.

        Return the list of application names that are available in the
        database.

        Returns:
            set of str.
        """
        app_names = set()
        for name in self.apps:
            app_names.add(name)

        return app_names

    def get_pretty_app_names(self):
        """
        Return the list of pretty app names that are available in the database.

        Returns:
            set of str.
        """
        pretty_app_names = set()
        for app_name in self.get_app_names():
            pretty_app_names.add(self.get_name(app_name))

        return pretty_app_names


class Config(object):

    """The Mackup Config class."""

    def __init__(self, filename=None):
        """
        Create a Config instance.

        Args:
            filename (str): Optional filename of the config file. If empty,
                            defaults to MACKUP_CONFIG_FILE
        """
        assert isinstance(filename, str) or filename is None

        # Initialize the parser
        self._parser = self._setup_parser(filename)

        # Do we have an old config file ?
        self._warn_on_old_config()

        # Get the storage engine
        self._engine = self._parse_engine()

        # Get the path where the Mackup folder is
        self._path = self._parse_path()

        # Get the directory replacing 'Mackup', if any
        self._directory = self._parse_directory()

        # Get the list of apps to ignore
        self._apps_to_ignore = self._parse_apps_to_ignore()

        # Get the list of apps to allow
        self._apps_to_sync = self._parse_apps_to_sync()

    @property
    def engine(self):
        """
        The engine used by the storage.

        ENGINE_DROPBOX, ENGINE_GDRIVE, ENGINE_COPY, ENGINE_ICLOUD, ENGINE_BOX
        or ENGINE_FS.

        Returns:
            str
        """
        return str(self._engine)

    @property
    def path(self):
        """
        Path to the Mackup configuration files.

        The path to the directory where Mackup is gonna create and store his
        directory.

        Returns:
            str
        """
        return str(self._path)

    @property
    def directory(self):
        """
        The name of the Mackup directory, named Mackup by default.

        Returns:
            str
        """
        return str(self._directory)

    @property
    def fullpath(self):
        """
        Full path to the Mackup configuration files.

        The full path to the directory when Mackup is storing the configuration
        files.

        Returns:
            str
        """
        return str(os.path.join(self.path, self.directory))

    @property
    def apps_to_ignore(self):
        """
        Get the list of applications ignored in the config file.

        Returns:
            set. Set of application names to ignore, lowercase
        """
        return set(self._apps_to_ignore)

    @property
    def apps_to_sync(self):
        """
        Get the list of applications allowed in the config file.

        Returns:
            set. Set of application names to allow, lowercase
        """
        return set(self._apps_to_sync)

    def _setup_parser(self, filename=None):
        """
        Configure the ConfigParser instance the way we want it.

        Args:
            filename (str) or None

        Returns:
            SafeConfigParser
        """
        assert isinstance(filename, str) or filename is None

        # If we are not overriding the config filename
        if not filename:
            filename = MACKUP_CONFIG_FILE

        parser = configparser.ConfigParser(allow_no_value=True)
        parser.read(os.path.join(os.path.join(os.environ['HOME'], filename)))

        return parser

    def _warn_on_old_config(self):
        """Warn the user if an old config format is detected."""
        # Is an old setion is in the config file ?
        old_sections = ['Allowed Applications', 'Ignored Applications']
        for old_section in old_sections:
            if self._parser.has_section(old_section):
                error("Old config file detected. Aborting.\n"
                      "\n"
                      "An old section (e.g. [Allowed Applications]"
                      " or [Ignored Applications] has been detected"
                      " in your {} file.\n"
                      "I'd rather do nothing than do something you"
                      " do not want me to do.\n"
                      "\n"
                      "Please read the up to date documentation on"
                      " <https://github.com/lra/mackup> and migrate"
                      " your configuration file."
                      .format(MACKUP_CONFIG_FILE))

    def _parse_engine(self):
        """
        Parse the storage engine in the config.

        Returns:
            str
        """
        if self._parser.has_option('storage', 'engine'):
            engine = str(self._parser.get('storage', 'engine'))
        else:
            engine = ENGINE_DROPBOX

        assert isinstance(engine, str)

        if engine not in [ENGINE_DROPBOX,
                          ENGINE_GDRIVE,
                          ENGINE_COPY,
                          ENGINE_ICLOUD,
                          ENGINE_BOX,
                          ENGINE_FS]:
            raise ConfigError('Unknown storage engine: {}'.format(engine))

        return str(engine)

    def _parse_path(self):
        """
        Parse the storage path in the config.

        Returns:
            str
        """
        if self.engine == ENGINE_DROPBOX:
            path = get_dropbox_folder_location()
        elif self.engine == ENGINE_GDRIVE:
            path = get_google_drive_folder_location()
        elif self.engine == ENGINE_COPY:
            path = get_copy_folder_location()
        elif self.engine == ENGINE_ICLOUD:
            path = get_icloud_folder_location()
        elif self.engine == ENGINE_BOX:
            path = get_box_folder_location()
        elif self.engine == ENGINE_FS:
            if self._parser.has_option('storage', 'path'):
                cfg_path = self._parser.get('storage', 'path')
                path = os.path.join(os.environ['HOME'], cfg_path)
            else:
                raise ConfigError("The required 'path' can't be found while"
                                  " the 'file_system' engine is used.")

        return str(path)

    def _parse_directory(self):
        """
        Parse the storage directory in the config.

        Returns:
            str
        """
        if self._parser.has_option('storage', 'directory'):
            directory = self._parser.get('storage', 'directory')
            # Don't allow CUSTOM_APPS_DIR as a storage directory
            if directory == CUSTOM_APPS_DIR:
                raise ConfigError("{} cannot be used as a storage directory."
                                  .format(CUSTOM_APPS_DIR))
        else:
            directory = MACKUP_BACKUP_PATH

        return str(directory)

    def _parse_apps_to_ignore(self):
        """
        Parse the applications to ignore in the config.

        Returns:
            set
        """
        # We ignore nothing by default
        apps_to_ignore = set()

        # Is the "[applications_to_ignore]" in the cfg file ?
        section_title = 'applications_to_ignore'
        if self._parser.has_section(section_title):
            apps_to_ignore = set(self._parser.options(section_title))

        return apps_to_ignore

    def _parse_apps_to_sync(self):
        """
        Parse the applications to backup in the config.

        Returns:
            set
        """
        # We allow nothing by default
        apps_to_sync = set()

        # Is the "[applications_to_sync]" section in the cfg file ?
        section_title = 'applications_to_sync'
        if self._parser.has_section(section_title):
            apps_to_sync = set(self._parser.options(section_title))

        return apps_to_sync


class ConfigError(Exception):

    """Exception used for handle errors in the configuration."""

    pass


class Mackup(object):

    """Main Mackup class."""

    def __init__(self):
        """Mackup Constructor."""
        self._config = Config()

        self.mackup_folder = self._config.fullpath
        self.temp_folder = tempfile.mkdtemp(prefix="mackup_tmp_")

    def check_for_usable_environment(self):
        """Check if the current env is usable and has everything's required."""
        # Do not let the user run Mackup as root
        if os.geteuid() == 0:
            error("Running Mackup as a superuser is useless and"
                        " dangerous. Don't do it!")

        # Do we have a folder to put the Mackup folder ?
        if not os.path.isdir(self._config.path):
            error("Unable to find the storage folder: {}"
                        .format(self._config.path))

        # Is Sublime Text running ?
        # if is_process_running('Sublime Text'):
        #    error("Sublime Text is running. It is known to cause problems"
        #          " when Sublime Text is running while I backup or restore"
        #          " its configuration files. Please close Sublime Text and"
        #          " run me again.")

    def check_for_usable_backup_env(self):
        """Check if the current env can be used to back up files."""
        self.check_for_usable_environment()
        self.create_mackup_home()

    def check_for_usable_restore_env(self):
        """Check if the current env can be used to restore files."""
        self.check_for_usable_environment()

        if not os.path.isdir(self.mackup_folder):
            error("Unable to find the Mackup folder: {}\n"
                        "You might want to back up some files or get your"
                        " storage directory synced first."
                        .format(self.mackup_folder))

    def clean_temp_folder(self):
        """Delete the temp folder and files created while running."""
        shutil.rmtree(self.temp_folder)

    def create_mackup_home(self):
        """If the Mackup home folder does not exist, create it."""
        if not os.path.isdir(self.mackup_folder):
            if confirm("Mackup needs a directory to store your"
                             " configuration files\n"
                             "Do you want to create it now? <{}>"
                             .format(self.mackup_folder)):
                os.makedirs(self.mackup_folder)
            else:
                error("Mackup can't do anything without a home =(")

    def get_apps_to_backup(self):
        """
        Get the list of applications that should be backed up by Mackup.

        It's the list of allowed apps minus the list of ignored apps.

        Returns:
            (set) List of application names to back up
        """
        # Instantiate the app db
        app_db = ApplicationsDatabase()

        # If a list of apps to sync is specify, we only allow those
        # Or we allow every supported app by default
        apps_to_backup = self._config.apps_to_sync or app_db.get_app_names()

        # Remove the specified apps to ignore
        for app_name in self._config.apps_to_ignore:
            apps_to_backup.discard(app_name)

        return apps_to_backup


class ColorFormatCodes:
    BLUE = '\033[34m'
    BOLD = '\033[1m'
    NORMAL = '\033[0m'


def header(str):
    return ColorFormatCodes.BLUE + str + ColorFormatCodes.NORMAL


def bold(str):
    return ColorFormatCodes.BOLD + str + ColorFormatCodes.NORMAL


def main():
    """Main function."""
    # Get the command line arg
    args = docopt(__doc__, version="Mackup {}".format(VERSION))

    mckp = Mackup()
    app_db = ApplicationsDatabase()

    def printAppHeader(app_name):
        if verbose:
            print(("\n{0} {1} {0}").format(header("---"), bold(app_name)))

    # If we want to answer mackup with "yes" for each question
    if args['--force']:
        FORCE_YES = True

    dry_run = args['--dry-run']

    verbose = args['--verbose']

    if args['backup']:
        # Check the env where the command is being run
        mckp.check_for_usable_backup_env()

        # Backup each application
        for app_name in sorted(mckp.get_apps_to_backup()):
            app = ApplicationProfile(mckp,
                                     app_db.get_files(app_name),
                                     dry_run,
                                     verbose)
            printAppHeader(app_name)
            app.backup()

    elif args['restore']:
        # Check the env where the command is being run
        mckp.check_for_usable_restore_env()

        # Restore the Mackup config before any other config, as we might need
        # it to know about custom settings
        # mackup_app = ApplicationProfile(mckp,
        #                                 app_db.get_files(MACKUP_APP_NAME),
        #                                 dry_run,
        #                                 verbose)
        # printAppHeader(MACKUP_APP_NAME)
        # mackup_app.restore()

        # Initialize again the apps db, as the Mackup config might have changed
        # it
        mckp = Mackup()
        app_db = ApplicationsDatabase()

        # Restore the rest of the app configs, using the restored Mackup config
        app_names = mckp.get_apps_to_backup()
        # Mackup has already been done
        app_names.discard(MACKUP_APP_NAME)

        for app_name in sorted(app_names):
            app = ApplicationProfile(mckp,
                                     app_db.get_files(app_name),
                                     dry_run,
                                     verbose)
            printAppHeader(app_name)
            app.restore()

    elif args['uninstall']:
        # Check the env where the command is being run
        mckp.check_for_usable_restore_env()

        if dry_run or (
           confirm("You are going to uninstall Mackup.\n"
                         "Every configuration file, setting and dotfile"
                         " managed by Mackup will be unlinked and moved back"
                         " to their original place, in your home folder.\n"
                         "Are you sure ?")):

            # Uninstall the apps except Mackup, which we'll uninstall last, to
            # keep the settings as long as possible
            app_names = mckp.get_apps_to_backup()
            app_names.discard(MACKUP_APP_NAME)

            for app_name in sorted(app_names):
                app = ApplicationProfile(mckp,
                                         app_db.get_files(app_name),
                                         dry_run,
                                         verbose)
                printAppHeader(app_name)
                app.uninstall()

            # Restore the Mackup config before any other config, as we might
            # need it to know about custom settings
            mackup_app = ApplicationProfile(mckp,
                                            app_db.get_files(MACKUP_APP_NAME),
                                            dry_run,
                                            verbose)
            mackup_app.uninstall()

            # Delete the Mackup folder in Dropbox
            # Don't delete this as there might be other Macs that aren't
            # uninstalled yet
            # delete(mckp.mackup_folder)

            print("\n"
                  "All your files have been put back into place. You can now"
                  " safely uninstall Mackup.\n"
                  "\n"
                  "Thanks for using Mackup !")

    elif args['list']:
        # Display the list of supported applications
        mckp.check_for_usable_environment()
        output = "Supported applications:\n"
        for app_name in sorted(app_db.get_app_names()):
            output += " - {}\n".format(app_name)
        output += "\n"
        output += ("{} applications supported in Mackup v{}"
                   .format(len(app_db.get_app_names()), VERSION))
        print(output)

    # Delete the tmp folder
    mckp.clean_temp_folder()


if __name__ == "__main__":
    # main(get_arguments())
    main()
