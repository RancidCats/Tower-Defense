using UnityEngine;
using UnityEngine.SceneManagement;
public class LevelCompleted : MonoBehaviour
{
    public void Exit()
    {
        Application.Quit();
    }
    public void NextLevel()
    {
        SceneManager.LoadScene(SceneManager.GetActiveScene().buildIndex+1);
    }
    public void Menu()
    {
        SceneManager.LoadScene(0);
    }
}
