using UnityEngine;
using UnityEngine.SceneManagement;

public class Pause : MonoBehaviour
{
    public void Continued()
    {
        Gamemanager.instance.Unpause();
        Gamemanager.instance.pauseUI.SetActive(false);
    }
    public void Menu()
    {
        Gamemanager.instance.Unpause();
        SceneManager.LoadScene(0);
        Gamemanager.instance.pauseUI.SetActive(false);
    }
    public void Exit()
    {
        Gamemanager.instance.Unpause();
        Application.Quit();
        Gamemanager.instance.pauseUI.SetActive(false);
    }
}
